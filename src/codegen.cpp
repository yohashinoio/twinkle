/**
 * codegen_cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2021 Hiramoto Ittou.
 */

#include "codegen.hpp"
#include "utility.hpp"

namespace miko::codegen
{

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

struct symbol_table {
  [[nodiscard]] llvm::AllocaInst*
  operator[](const std::string& name) const noexcept
  try {
    return named_values.at(name);
  }
  catch (const std::out_of_range&) {
    return nullptr;
  }

  // regist stands for register.
  void regist(const std::string& name, llvm::AllocaInst* value)
  {
    named_values.insert({name, value});
  }

  bool exists(const std::string& name)
  {
    return named_values.count(name);
  }

  // For debug.
  void print_symbols() const
  {
    for (auto&& r : named_values) {
      std::cout << r.first << ' ';
    }
    std::endl(std::cout);
  }

private:
  std::unordered_map<std::string, llvm::AllocaInst*> named_values;
};

// Create an alloca instruction in the entry block of
// the function.  This is used for mutable variables etc.
[[nodiscard]] llvm::AllocaInst*
create_entry_block_alloca(llvm::Function*    func,
                          llvm::LLVMContext& context,
                          const std::string& var_name)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());

  return tmp.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, var_name);
}

//===----------------------------------------------------------------------===//
// Visitors
//===----------------------------------------------------------------------===//

// It seems that the member function has to be const.
struct expression_visitor : public boost::static_visitor<llvm::Value*> {
  expression_visitor(std::shared_ptr<llvm::Module> module,
                     llvm::IRBuilder<>&            builder,
                     const std::filesystem::path&  file_path,
                     symbol_table&                 scope)
    : module{module}
    , builder{builder}
    , file_path{file_path}
    , scope{scope}
  {
  }

  llvm::Value* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  llvm::Value* operator()(const int value) const
  {
    return llvm::ConstantInt::get(builder.getInt32Ty(), value);
  }

  llvm::Value* operator()(const ast::unary_op_expr& node) const
  {
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to 0-x
      return builder.CreateSub(llvm::ConstantInt::get(builder.getInt32Ty(), 0),
                               rhs);
    }

    BOOST_ASSERT_MSG(
      0,
      "unsupported unary operators may have been converted to ASTs.");
  }

  llvm::Value* operator()(const ast::binary_op_expr& node) const
  {
    // Special case assignment because we don't want to emit the left-hand-side
    // as an expression.
    if (node.op == "=") {
      try {
        auto&& lhs = boost::get<ast::variable_expr>(node.lhs);

        auto* rhs = boost::apply_visitor(*this, node.rhs);
        if (!rhs)
          return nullptr;

        auto* variable = scope[lhs.name];

        if (!variable) {
          throw std::runtime_error{
            format_error_message(file_path.string(),
                                 format("unknown variable name %s", lhs.name))};
        }

        builder.CreateStore(rhs, variable);
        return rhs;
      }
      catch (const boost::bad_get&) {
        // left hand side was not a variable.
        throw std::runtime_error{format_error_message(
          file_path.string(),
          "the left hand side of the assignment must be a variable")};
      }
    }

    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);
    if (!lhs || !rhs)
      return nullptr;

    // addition
    if (node.op == "+")
      return builder.CreateAdd(lhs, rhs);
    if (node.op == "-")
      return builder.CreateSub(lhs, rhs);

    // multiplication
    if (node.op == "*")
      return builder.CreateMul(lhs, rhs);
    if (node.op == "/")
      return builder.CreateSDiv(lhs, rhs);

    // equality
    if (node.op == "==")
      return builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhs, rhs);
    if (node.op == "!=")
      return builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhs, rhs);

    // relational
    if (node.op == "<")
      return builder.CreateICmp(llvm::ICmpInst::ICMP_SLT, lhs, rhs);
    if (node.op == ">")
      return builder.CreateICmp(llvm::ICmpInst::ICMP_SGT, lhs, rhs);
    if (node.op == "<=")
      return builder.CreateICmp(llvm::ICmpInst::ICMP_SLE, lhs, rhs);
    if (node.op == ">=")
      return builder.CreateICmp(llvm::ICmpInst::ICMP_SGE, lhs, rhs);

    BOOST_ASSERT_MSG(
      0,
      "unsupported binary operators may have been converted to ASTs.");
  }

  llvm::Value* operator()(const ast::variable_expr& node) const
  {
    auto* ainst = scope[node.name];

    if (!ainst) {
      throw std::runtime_error{format_error_message(
        file_path.string(),
        format("unknown variable '%s' referenced", node.name))};
    }

    return builder.CreateLoad(ainst->getAllocatedType(),
                              ainst,
                              node.name.c_str());
  }

  llvm::Value* operator()(const ast::function_call_expr& node) const
  {
    auto* callee_f = module->getFunction(node.callee);

    if (!callee_f) {
      throw std::runtime_error{format_error_message(
        file_path.string(),
        format("unknown function '%s' referenced", node.callee))};
    }

    if (callee_f->arg_size() != node.args.size()) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             format("incorrect arguments passed"))};
    }

    std::vector<llvm::Value*> args_value;
    for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
      args_value.push_back(boost::apply_visitor(*this, node.args[i]));

      if (!args_value.back())
        return nullptr;
    }

    return builder.CreateCall(callee_f, args_value);
  }

private:
  std::shared_ptr<llvm::Module> module;
  llvm::IRBuilder<>&            builder;

  const std::filesystem::path& file_path;

  symbol_table& scope;
};

void codegen_compound_statement(const ast::compound_statement& statements,
                                const symbol_table&            table,
                                llvm::LLVMContext&             context,
                                std::shared_ptr<llvm::Module>  module,
                                llvm::IRBuilder<>&             builder,
                                const std::filesystem::path&   file_path,
                                llvm::AllocaInst*              alloca_ret,
                                llvm::BasicBlock*              end_bb);

struct statement_visitor : public boost::static_visitor<void> {
  statement_visitor(llvm::LLVMContext&            context,
                    std::shared_ptr<llvm::Module> module,
                    llvm::IRBuilder<>&            builder,
                    const std::filesystem::path&  file_path,
                    symbol_table&                 scope,
                    llvm::AllocaInst*             alloca_ret,
                    llvm::BasicBlock*             end_bb)
    : context{context}
    , module{module}
    , builder{builder}
    , file_path{file_path}
    , scope{scope}
    , alloca_ret{alloca_ret}
    , end_bb{end_bb}
  {
  }

  void operator()(ast::nil) const
  {
    // empty statement.
  }

  void operator()(const ast::expression& node) const
  {
    if (!boost::apply_visitor(
          expression_visitor{module, builder, file_path, scope},
          node)) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             "failed to generate expression code")};
    }
  }

  void operator()(const ast::return_statement& node) const
  {
    auto* retval = boost::apply_visitor(
      expression_visitor{module, builder, file_path, scope},
      node.rhs);

    if (!retval) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             "failed to generate return value")};
    }

    builder.CreateStore(retval, alloca_ret);
    builder.CreateBr(end_bb);
  }

  void operator()(const ast::variable_def_statement& node) const
  {
    if (scope.exists(node.name)) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             format("redefinition of '%s'", node.name))};
    }

    auto* func = builder.GetInsertBlock()->getParent();

    auto* ainst = create_entry_block_alloca(func, context, node.name);

    if (node.initializer) {
      auto* initializer = boost::apply_visitor(
        expression_visitor{module, builder, file_path, scope},
        *node.initializer);

      if (!initializer) {
        throw std::runtime_error{format_error_message(
          file_path.string(),
          format("initialization of variable %s failed", node.name))};
      }

      builder.CreateStore(initializer, ainst);
    }

    scope.regist(node.name, ainst);
  }

  void operator()(const ast::if_statement& node) const
  {
    auto* condition_value = boost::apply_visitor(
      expression_visitor{module, builder, file_path, scope},
      node.condition);

    if (!condition_value) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             "invalid condition in if statement")};
    }

    // Convert condition to a bool by comparing non-equal to 0.
    condition_value
      = builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                           condition_value,
                           llvm::ConstantInt::get(builder.getInt1Ty(), 0));

    auto* func = builder.GetInsertBlock()->getParent();

    auto* then_bb = llvm::BasicBlock::Create(context, "if.then", func);
    auto* else_bb = llvm::BasicBlock::Create(context, "if.else");

    auto* merge_bb = llvm::BasicBlock::Create(context, "if.merge");

    builder.CreateCondBr(condition_value, then_bb, else_bb);

    // then statement codegen.
    builder.SetInsertPoint(then_bb);

    codegen_compound_statement(node.then_statement,
                               scope,
                               context,
                               module,
                               builder,
                               file_path,
                               alloca_ret,
                               end_bb);

    if (!builder.GetInsertBlock()->getTerminator())
      builder.CreateBr(merge_bb);

    // else statement codegen.
    func->getBasicBlockList().push_back(else_bb);
    builder.SetInsertPoint(else_bb);

    if (node.else_statement) {
      codegen_compound_statement(*node.else_statement,
                                 scope,
                                 context,
                                 module,
                                 builder,
                                 file_path,
                                 alloca_ret,
                                 end_bb);
    }

    if (!builder.GetInsertBlock()->getTerminator())
      builder.CreateBr(merge_bb);

    func->getBasicBlockList().push_back(merge_bb);
    builder.SetInsertPoint(merge_bb);
  }

  void operator()(const ast::for_statement& node) const
  {
    if (node.init_expression) {
      auto* init_value = boost::apply_visitor(
        expression_visitor{module, builder, file_path, scope},
        *node.init_expression);

      if (!init_value) {
        throw std::runtime_error{format_error_message(
          file_path.string(),
          "failed to generate init expression in for statement")};
      }
    }

    auto* func = builder.GetInsertBlock()->getParent();

    auto* cond_bb = llvm::BasicBlock::Create(context, "for.cond", func);
    auto* loop_bb = llvm::BasicBlock::Create(context, "for.loop");
    auto* body_bb = llvm::BasicBlock::Create(context, "for.body");

    auto* for_end_bb = llvm::BasicBlock::Create(context, "for.end");

    builder.CreateBr(cond_bb);
    builder.SetInsertPoint(cond_bb);

    if (node.cond_expression) {
      auto* cond_value = boost::apply_visitor(
        expression_visitor{module, builder, file_path, scope},
        *node.cond_expression);

      if (!cond_value) {
        throw std::runtime_error{format_error_message(
          file_path.string(),
          "failed to generate condition expression in for statement")};
      }

      cond_value
        = builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                             cond_value,
                             llvm::ConstantInt::get(builder.getInt1Ty(), 0));

      builder.CreateCondBr(cond_value, body_bb, for_end_bb);
    }
    else {
      // If condition is absent, unconditionally true.
      builder.CreateCondBr(llvm::ConstantInt::get(builder.getInt1Ty(), true),
                           body_bb,
                           for_end_bb);
    }

    func->getBasicBlockList().push_back(body_bb);
    builder.SetInsertPoint(body_bb);

    codegen_compound_statement(node.body,
                               scope,
                               context,
                               module,
                               builder,
                               file_path,
                               alloca_ret,
                               end_bb);

    builder.CreateBr(loop_bb);

    func->getBasicBlockList().push_back(loop_bb);
    builder.SetInsertPoint(loop_bb);

    if (node.loop_expression) {
      auto* loop_value = boost::apply_visitor(
        expression_visitor{module, builder, file_path, scope},
        *node.loop_expression);

      if (!loop_value) {
        throw std::runtime_error{format_error_message(
          file_path.string(),
          "failed to generate loop expression in for statement")};
      }
    }

    builder.CreateBr(cond_bb);

    func->getBasicBlockList().push_back(for_end_bb);
    builder.SetInsertPoint(for_end_bb);
  }

private:
  llvm::LLVMContext&            context;
  std::shared_ptr<llvm::Module> module;
  llvm::IRBuilder<>&            builder;

  const std::filesystem::path& file_path;

  symbol_table& scope;

  llvm::AllocaInst* alloca_ret;
  llvm::BasicBlock* end_bb;
};

void codegen_compound_statement(const ast::compound_statement& statements,
                                const symbol_table&            table,
                                llvm::LLVMContext&             context,
                                std::shared_ptr<llvm::Module>  module,
                                llvm::IRBuilder<>&             builder,
                                const std::filesystem::path&   file_path,
                                llvm::AllocaInst*              alloca_ret,
                                llvm::BasicBlock*              end_bb)
{
  symbol_table scope = table;

  for (auto&& statement : statements) {
    // If there is already a Terminator, the code generation of the main
    // body of the function is terminated on the spot.
    if (builder.GetInsertBlock()->getTerminator())
      break;

    boost::apply_visitor(statement_visitor{context,
                                           module,
                                           builder,
                                           file_path,
                                           scope,
                                           alloca_ret,
                                           end_bb},
                         statement);
  }
}

struct program_visitor : public boost::static_visitor<llvm::Function*> {
  program_visitor(llvm::LLVMContext&                 context,
                  std::shared_ptr<llvm::Module>      module,
                  llvm::IRBuilder<>&                 builder,
                  const std::filesystem::path&       file_path,
                  llvm::legacy::FunctionPassManager& fpm)
    : context{context}
    , module{module}
    , builder{builder}
    , file_path{file_path}
    , fpm{fpm}
  {
  }

  llvm::Function* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  // Function declaration
  llvm::Function* operator()(const ast::function_declare& node) const
  {
    std::vector<llvm::Type*> param_types(node.args.size(),
                                         builder.getInt32Ty());

    auto* func_type
      = llvm::FunctionType::get(builder.getInt32Ty(), param_types, false);

    auto* func = llvm::Function::Create(func_type,
                                        llvm::Function::ExternalLinkage,
                                        node.name,
                                        module.get());

    // Set names for all arguments.
    {
      std::size_t idx = 0;
      for (auto&& arg : func->args())
        arg.setName(node.args[idx++]);
    }

    return func;
  }

  // Function definition
  llvm::Function* operator()(const ast::function_define& node) const
  {
    auto* func = module->getFunction(node.decl.name);

    if (!func)
      func = this->operator()(node.decl);

    if (!func) {
      throw std::runtime_error{format_error_message(
        file_path.string(),
        format("failed to create function %s", node.decl.name),
        true)};
    }

    symbol_table argument_values;

    auto* entry_bb = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry_bb);

    for (auto& arg : func->args()) {
      // Create an alloca for this variable.
      auto* alloca
        = create_entry_block_alloca(func, context, arg.getName().str());

      // Store the initial value into the alloca.
      builder.CreateStore(&arg, alloca);

      // Add arguments to variable symbol table.
      argument_values.regist(arg.getName().str(), alloca);
    }

    auto* alloca_ret  = create_entry_block_alloca(func, context, "retval");
    auto* func_end_bb = llvm::BasicBlock::Create(context, "end");

    codegen_compound_statement(node.body,
                               argument_values,
                               context,
                               module,
                               builder,
                               file_path,
                               alloca_ret,
                               func_end_bb);

    // Return 0 if main function has no return.
    if (node.decl.name == "main"
        && !builder.GetInsertBlock()->getTerminator()) {
      builder.CreateStore(llvm::ConstantInt::get(builder.getInt32Ty(), 0),
                          alloca_ret);
      builder.CreateBr(func_end_bb);
    }

    func->getBasicBlockList().push_back(func_end_bb);
    builder.SetInsertPoint(func_end_bb);

    auto* retval
      = builder.CreateLoad(alloca_ret->getAllocatedType(), alloca_ret);
    builder.CreateRet(retval);

    std::string              em;
    llvm::raw_string_ostream os{em};
    if (llvm::verifyFunction(*func, &os)) {
      func->eraseFromParent();

      throw std::runtime_error{
        format_error_message(file_path.string(), os.str())};
    }

    fpm.run(*func);

    return func;
  }

private:
  llvm::LLVMContext&            context;
  std::shared_ptr<llvm::Module> module;
  llvm::IRBuilder<>&            builder;

  const std::filesystem::path& file_path;

  llvm::legacy::FunctionPassManager& fpm;
};

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

code_generator::code_generator(const ast::program&          ast,
                               const position_cache&        positions,
                               const std::filesystem::path& file_path,
                               const bool                   optimize)
  : module{std::make_shared<llvm::Module>(file_path.filename().string(),
                                          context)}
  , builder{context}
  , fpm{module.get()}
  , file_path{file_path}
  , ast{ast}
  , positions{positions}
{
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  if (optimize) {
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    fpm.add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    fpm.add(llvm::createReassociatePass());
    // Eliminate Common SubExpressions.
    fpm.add(llvm::createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    fpm.add(llvm::createCFGSimplificationPass());
    // Promote allocas to registers.
    fpm.add(llvm::createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    fpm.add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    fpm.add(llvm::createReassociatePass());
  }

  fpm.doInitialization();

  // set target triple and data layout to module
  const auto target_triple = llvm::sys::getDefaultTargetTriple();

  std::string target_triple_error;
  auto        target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_error);

  if (!target) {
    throw std::runtime_error{
      format_error_message("mikoc",
                           format("failed to lookup target %s: %s",
                                  target_triple,
                                  target_triple_error),
                           true)};
  }

  llvm::TargetOptions target_options;
  target_machine
    = target->createTargetMachine(target_triple,
                                  "generic",
                                  "",
                                  target_options,
                                  llvm::Optional<llvm::Reloc::Model>());

  module->setTargetTriple(target_triple);
  module->setDataLayout(target_machine->createDataLayout());

  codegen();
}

void code_generator::write_llvm_ir_to_file(
  const std::filesystem::path& out) const
{
  std::error_code      ostream_ec;
  llvm::raw_fd_ostream os{out.string(),
                          ostream_ec,
                          llvm::sys::fs::OpenFlags::OF_None};

  if (ostream_ec) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("%s: %s", out.string(), ostream_ec.message()))};
  }

  module->print(os, nullptr);
}

void code_generator::write_object_code_to_file(
  const std::filesystem::path& out) const
{
  std::error_code      ostream_ec;
  llvm::raw_fd_ostream os{out.string(),
                          ostream_ec,
                          llvm::sys::fs::OpenFlags::OF_None};
  if (ostream_ec) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("%s: %s\n", out.string(), ostream_ec.message()))};
  }

  llvm::legacy::PassManager pm;
  if (target_machine->addPassesToEmitFile(pm,
                                          os,
                                          nullptr,
                                          llvm::CGFT_ObjectFile)) {
    throw std::runtime_error{
      format_error_message("mikoc",
                           "targetMachine can't emit a file of this types",
                           true)};
  }

  pm.run(*module);
  os.flush();
}

void code_generator::codegen()
{
  for (auto&& node : ast) {
    boost::apply_visitor(
      program_visitor{context, module, builder, file_path, fpm},
      node);
  }
}

} // namespace miko::codegen
