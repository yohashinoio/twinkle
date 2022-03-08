//
//  codegen.cpp
//
//  Copyright (c) 2022 The Miko Authors.
//  Apache License v2.0
//

#include "codegen.hpp"
#include "utility.hpp"

namespace miko::codegen
{

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

// It seems that the member function has to be const.
struct expression_visitor : public boost::static_visitor<llvm::Value*> {
  expression_visitor(std::shared_ptr<llvm::Module> module,
                     llvm::IRBuilder<>&            builder,
                     symbol_table&                 named_values,
                     const std::filesystem::path&  file_path)
    : module{module}
    , builder{builder}
    , named_values{named_values}
    , file_path{file_path}
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

  llvm::Value* operator()(const ast::unaryop& node) const
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

  llvm::Value* operator()(const ast::binop& node) const
  {
    // Special case assignment because we don't want to emit the left-hand-side
    // as an expression.
    if (node.op == "=") {
      try {
        auto&& lhs = boost::get<ast::variable>(node.lhs);

        auto* rhs = boost::apply_visitor(*this, node.rhs);
        if (!rhs)
          return nullptr;

        auto* variable = named_values[lhs.name];

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

  llvm::Value* operator()(const ast::variable& node) const
  {
    auto* ainst = named_values[node.name];
    if (!ainst) {
      throw std::runtime_error{format_error_message(
        file_path.string(),
        format("unknown variable '%s' referenced", node.name))};
    }

    return builder.CreateLoad(ainst->getAllocatedType(),
                              ainst,
                              node.name.c_str());
  }

  llvm::Value* operator()(const ast::function_call& node) const
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

  symbol_table& named_values;

  const std::filesystem::path& file_path;
};

struct statement_visitor : public boost::static_visitor<void> {
  statement_visitor(llvm::LLVMContext&            context,
                    std::shared_ptr<llvm::Module> module,
                    llvm::IRBuilder<>&            builder,
                    symbol_table&                 named_values,
                    const std::filesystem::path&  file_path)
    : context{context}
    , module{module}
    , builder{builder}
    , named_values{named_values}
    , file_path{file_path}
  {
  }

  void operator()(ast::nil) const
  {
    // empty statement.
  }

  void operator()(const ast::expression& node) const
  {
    if (!boost::apply_visitor(
          expression_visitor{module, builder, named_values, file_path},
          node)) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             "failed to generate expression code")};
    }
  }

  void operator()(const ast::return_statement& node) const
  {
    auto* retval = boost::apply_visitor(
      expression_visitor{module, builder, named_values, file_path},
      node.rhs);

    if (!retval) {
      throw std::runtime_error{
        format_error_message(file_path.string(),
                             "failure to generate return value")};
    }

    builder.CreateRet(retval);
  }

  void operator()(const ast::variable_def& node) const
  {
    auto* func = builder.GetInsertBlock()->getParent();

    auto* ainst = create_entry_block_alloca(func, context, node.name);

    if (node.initializer) {
      auto* initializer = boost::apply_visitor(
        expression_visitor{module, builder, named_values, file_path},
        *node.initializer);

      if (!initializer) {
        throw std::runtime_error{format_error_message(
          file_path.string(),
          format("initialization of variable %s failed", node.name))};
      }

      builder.CreateStore(initializer, ainst);
    }
    else {
      builder.CreateStore(llvm::ConstantInt::get(builder.getInt32Ty(), 0),
                          ainst);
    }

    named_values.regist(node.name, ainst);
  }

  void operator()(const ast::if_statement& node) const
  {
    auto* condition_value = boost::apply_visitor(
      expression_visitor{module, builder, named_values, file_path},
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

    auto* retval_ainst = create_entry_block_alloca(func, context, "if_retval");

    auto* then_bb = llvm::BasicBlock::Create(context, "if.then", func);
    auto* else_bb = llvm::BasicBlock::Create(context, "if.else");

    // Used only when there is a return statement in an if statement
    auto* return_bb = llvm::BasicBlock::Create(context, "if.return");

    auto* end_bb = llvm::BasicBlock::Create(context, "if.end");

    builder.CreateCondBr(condition_value, then_bb, else_bb);

    // then statement codegen.
    builder.SetInsertPoint(then_bb);

    bool with_return = false;

    for (auto&& statement : node.then_statement) {
      if (statement.type() == typeid(ast::return_statement)) {
        with_return = true;

        auto&& return_node = boost::get<ast::return_statement>(statement);

        auto* retval = boost::apply_visitor(
          expression_visitor{module, builder, named_values, file_path},
          return_node.rhs);

        if (!retval) {
          throw std::runtime_error{
            format_error_message(file_path.string(),
                                 "failure to generate return value")};
        }

        builder.CreateStore(retval, retval_ainst);

        builder.CreateBr(return_bb);

        break;
      }

      boost::apply_visitor(
        statement_visitor{context, module, builder, named_values, file_path},
        statement);
    }

    // Generate a br to end_bb only if there is no terminator (such as a br to
    // return_bb).
    if (!then_bb->getTerminator())
      builder.CreateBr(end_bb);

    // else statement codegen.
    func->getBasicBlockList().push_back(else_bb);
    builder.SetInsertPoint(else_bb);

    if (node.else_statement) {
      for (auto&& statement : *node.else_statement) {
        if (statement.type() == typeid(ast::return_statement)) {
          with_return = true;

          auto&& return_node = boost::get<ast::return_statement>(statement);

          auto* retval = boost::apply_visitor(
            expression_visitor{module, builder, named_values, file_path},
            return_node.rhs);

          if (!retval) {
            throw std::runtime_error{
              format_error_message(file_path.string(),
                                   "failure to generate return value")};
          }

          builder.CreateStore(retval, retval_ainst);

          builder.CreateBr(return_bb);

          break;
        }

        boost::apply_visitor(
          statement_visitor{context, module, builder, named_values, file_path},
          statement);
      }
    }

    // Generate a br to end_bb only if there is no terminator (such as a br to
    // return_bb).
    if (!else_bb->getTerminator())
      builder.CreateBr(end_bb);

    if (with_return) {
      func->getBasicBlockList().push_back(return_bb);
      builder.SetInsertPoint(return_bb);

      auto* retval
        = builder.CreateLoad(retval_ainst->getAllocatedType(), retval_ainst);
      builder.CreateRet(retval);
    }

    func->getBasicBlockList().push_back(end_bb);
    builder.SetInsertPoint(end_bb);
  }

private:
  llvm::LLVMContext&            context;
  std::shared_ptr<llvm::Module> module;
  llvm::IRBuilder<>&            builder;

  symbol_table& named_values;

  const std::filesystem::path& file_path;
};

struct program_visitor : public boost::static_visitor<llvm::Function*> {
  program_visitor(llvm::LLVMContext&                 context,
                  std::shared_ptr<llvm::Module>      module,
                  llvm::IRBuilder<>&                 builder,
                  llvm::legacy::FunctionPassManager& fpm,
                  const std::filesystem::path&       file_path)
    : context{context}
    , module{module}
    , builder{builder}
    , fpm{fpm}
    , file_path{file_path}
  {
  }

  llvm::Function* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  // Function declaration
  llvm::Function* operator()(const ast::function_decl& node) const
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
  llvm::Function* operator()(const ast::function_def& node) const
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

    auto* basic_block = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(basic_block);

    symbol_table named_values;

    for (auto& arg : func->args()) {
      // Create an alloca for this variable.
      auto* alloca
        = create_entry_block_alloca(func, context, arg.getName().str());

      // Store the initial value into the alloca.
      builder.CreateStore(&arg, alloca);

      // Add arguments to variable symbol table.
      named_values.regist(arg.getName().str(), alloca);
    }

    for (auto&& statement : node.body) {
      // If there is already a Terminator, the code generation of the main body
      // of the function is terminated on the spot.
      if (builder.GetInsertBlock()->getTerminator())
        break;

      boost::apply_visitor(
        statement_visitor{context, module, builder, named_values, file_path},
        statement);
    }

    // Return 0 if main function has no return.
    if (node.decl.name == "main"
        && !builder.GetInsertBlock()->getTerminator()) {
      builder.CreateRet(llvm::ConstantInt::get(builder.getInt32Ty(), 0));
    }

    // DEBUG
    module->dump();

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

  llvm::legacy::FunctionPassManager& fpm;

  const std::filesystem::path& file_path;
};

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

  codegen();
}

void code_generator::write_llvm_ir_to_file(
  const std::filesystem::path& out) const
{
  // TODO: Add target triple

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
  const auto target_triple = llvm::sys::getDefaultTargetTriple();

  // Target triple error string.
  std::string target_triple_es;
  auto        target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_es);

  if (!target) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("failed to lookup target %s: %s", target_triple, target_triple_es),
      true)};
  }

  llvm::TargetOptions opt;
  auto*               the_target_machine
    = target->createTargetMachine(target_triple,
                                  "generic",
                                  "",
                                  opt,
                                  llvm::Optional<llvm::Reloc::Model>());

  module->setTargetTriple(target_triple);
  module->setDataLayout(the_target_machine->createDataLayout());

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
  if (the_target_machine->addPassesToEmitFile(pm,
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
      program_visitor{context, module, builder, fpm, file_path},
      node);
  }
}

} // namespace miko::codegen
