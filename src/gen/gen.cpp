/**
 * codegen_cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <gen/gen.hpp>
#include <parse/id.hpp>

namespace miko::codegen
{

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

struct variable_info {
  llvm::AllocaInst* instance;
  bool              is_mutable;
  bool              is_signed;
};

struct symbol_table {
  [[nodiscard]] std::optional<variable_info>
  operator[](const std::string& name) const noexcept
  try {
    return named_values.at(name);
  }
  catch (const std::out_of_range&) {
    return std::nullopt;
  }

  // regist stands for register.
  void regist(const std::string& name, variable_info info)
  {
    named_values.insert({name, info});
  }

  // Returns true if the variable is already registered, false otherwise.
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
  std::unordered_map<std::string, variable_info> named_values;
};

// Create an alloca instruction in the entry block of
// the function.  This is used for mutable variables etc.
[[nodiscard]] llvm::AllocaInst*
create_entry_block_alloca(llvm::Function*    func,
                          const std::string& var_name,
                          llvm::Type*        type)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());

  return tmp.CreateAlloca(type, nullptr, var_name);
}

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct expression_visitor : public boost::static_visitor<llvm::Value*> {
  expression_visitor(codegen_common& common, symbol_table& scope)
    : common{common}
    , scope{scope}
  {
  }

  llvm::Value* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  // unsigned integer literal
  llvm::Value* operator()(const std::uint32_t node) const
  {
    return llvm::ConstantInt::get(common.builder.getInt32Ty(), node);
  }

  // signed integer literal
  llvm::Value* operator()(const std::int32_t node) const
  {
    return llvm::ConstantInt::getSigned(common.builder.getInt32Ty(), node);
  }

  // string literal
  llvm::Value* operator()(const ast::string_literal& node) const
  {
    return common.builder.CreateGlobalStringPtr(node.str);
  }

  llvm::Value* operator()(const ast::unary_op_expr& node) const
  {
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to 0-x
      return common.builder.CreateSub(
        llvm::ConstantInt::get(common.builder.getInt32Ty(), 0),
        rhs);
    }

    BOOST_ASSERT_MSG(
      0,
      "unsupported unary operators may have been converted to ASTs.");
  }

  llvm::Value* operator()(const ast::binary_op_expr& node) const
  {
    // Special case assignment because we don't want to emit the
    // left-hand-side as an expression.
    if (node.op == "=") {
      try {
        auto&& lhs = boost::get<ast::variable_expr>(node.lhs);

        auto rhs = boost::apply_visitor(*this, node.rhs);
        if (!rhs)
          BOOST_ASSERT(0);

        auto var_info = scope[lhs.name];

        if (!var_info) {
          throw std::runtime_error{format_error_message(
            common.file.string(),
            format("unknown variable name '%s'", lhs.name))};
        }

        if (!var_info->is_mutable) {
          throw std::runtime_error{format_error_message(
            common.file.string(),
            format("assignment of read-only variable '%s'", lhs.name))};
        }

        common.builder.CreateStore(rhs, var_info->instance);
        return rhs;
      }
      catch (const boost::bad_get&) {
        // left hand side was not a variable.
        throw std::runtime_error{format_error_message(
          common.file.string(),
          "the left hand side of the assignment must be a variable")};
      }
    }

    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);
    if (!lhs || !rhs)
      BOOST_ASSERT(0);

    // addition
    if (node.op == "+")
      return common.builder.CreateAdd(lhs, rhs);
    if (node.op == "-")
      return common.builder.CreateSub(lhs, rhs);

    // multiplication
    if (node.op == "*")
      return common.builder.CreateMul(lhs, rhs);
    if (node.op == "/") {
      // TODO: unsigned
      return common.builder.CreateSDiv(lhs, rhs);
    }

    // equality
    if (node.op == "==")
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhs, rhs));
    if (node.op == "!=")
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhs, rhs));

    // relational
    if (node.op == "<")
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SLT, lhs, rhs));
    if (node.op == ">")
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SGT, lhs, rhs));
    if (node.op == "<=")
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SLE, lhs, rhs));
    if (node.op == ">=")
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SGE, lhs, rhs));

    BOOST_ASSERT_MSG(
      0,
      "unsupported binary operators may have been converted to ASTs.");
  }

  llvm::Value* operator()(const ast::variable_expr& node) const
  {
    auto var_info = scope[node.name];

    if (!var_info) {
      throw std::runtime_error{format_error_message(
        common.file.string(),
        format("unknown variable '%s' referenced", node.name))};
    }

    return common.builder.CreateLoad(var_info->instance->getAllocatedType(),
                                     var_info->instance,
                                     node.name.c_str());
  }

  llvm::Value* operator()(const ast::function_call_expr& node) const
  {
    auto callee_function = common.module.getFunction(node.callee);

    if (!callee_function) {
      throw std::runtime_error{format_error_message(
        common.file.string(),
        format("unknown function '%s' referenced", node.callee))};
    }

    if (callee_function->arg_size() != node.args.size()) {
      throw std::runtime_error{
        format_error_message(common.file.string(),
                             format("incorrect arguments passed"))};
    }

    std::vector<llvm::Value*> args_value;
    for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
      args_value.push_back(boost::apply_visitor(*this, node.args[i]));

      if (!args_value.back())
        BOOST_ASSERT(0);
    }

    return common.builder.CreateCall(callee_function, args_value);
  }

  llvm::Value* operator()(const ast::cast_expr& node) const
  {
    auto rhs = boost::apply_visitor(*this, node.rhs);
    if (!rhs)
      BOOST_ASSERT(0);

    auto as = common.typename_to_type(node.as);

    return common.builder.CreateIntCast(rhs, as.type, as.is_signed);

    return rhs;
  }

private:
  codegen_common& common;

  symbol_table& scope;
};

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

void codegen_compound_statement(const ast::compound_statement& statements,
                                const symbol_table&            scope,
                                codegen_common&                common,
                                llvm::AllocaInst*              retvar,
                                llvm::BasicBlock*              end_bb);

struct statement_visitor : public boost::static_visitor<void> {
  statement_visitor(codegen_common&   common,
                    symbol_table&     scope,
                    llvm::AllocaInst* retvar,
                    llvm::BasicBlock* end_bb)
    : common{common}
    , scope{scope}
    , retvar{retvar}
    , end_bb{end_bb}
  {
  }

  void operator()(ast::nil) const
  {
    // empty statement.
  }

  void operator()(const ast::expression& node) const
  {
    if (!boost::apply_visitor(expression_visitor{common, scope}, node)) {
      throw std::runtime_error{
        format_error_message(common.file.string(),
                             "failed to generate expression code")};
    }
  }

  void operator()(const ast::return_statement& node) const
  {
    if (node.rhs) {
      auto retval
        = boost::apply_visitor(expression_visitor{common, scope}, *node.rhs);

      if (!retval) {
        throw std::runtime_error{
          format_error_message(common.file.string(),
                               "failed to generate return value")};
      }

      common.builder.CreateStore(retval, retvar);
    }

    common.builder.CreateBr(end_bb);
  }

  void operator()(const ast::variable_def_statement& node) const
  {
    if (scope.exists(node.name)) {
      throw std::runtime_error{
        format_error_message(common.file.string(),
                             format("redefinition of '%s'", node.name))};
    }

    auto function = common.builder.GetInsertBlock()->getParent();

    auto type_info = common.typename_to_type(node.type);

    auto inst = create_entry_block_alloca(function, node.name, type_info.type);

    if (node.initializer) {
      auto initializer = boost::apply_visitor(expression_visitor{common, scope},
                                              *node.initializer);

      if (!initializer) {
        throw std::runtime_error{format_error_message(
          common.file.string(),
          format("initialization of variable %s failed", node.name))};
      }

      common.builder.CreateStore(initializer, inst);
    }

    if (!node.qualifier) {
      // consttant variable.
      scope.regist(node.name, {inst, false, type_info.is_signed});
    }
    else if (*node.qualifier == id::variable_qualifier::mutable_) {
      // mutable variable.
      scope.regist(node.name, {inst, true, type_info.is_signed});
    }
  }

  void operator()(const ast::if_statement& node) const
  {
    auto condition_value
      = boost::apply_visitor(expression_visitor{common, scope}, node.condition);

    if (!condition_value) {
      throw std::runtime_error{
        format_error_message(common.file.string(),
                             "invalid condition in if statement")};
    }

    // Convert condition to a bool by comparing non-equal to 0.
    condition_value = common.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      condition_value,
      llvm::ConstantInt::get(common.typename_to_type(id::type_name::bool_).type,
                             0));

    auto function = common.builder.GetInsertBlock()->getParent();

    auto then_bb
      = llvm::BasicBlock::Create(common.context, "if.then", function);
    auto else_bb = llvm::BasicBlock::Create(common.context, "if.else");

    auto merge_bb = llvm::BasicBlock::Create(common.context, "if.merge");

    common.builder.CreateCondBr(condition_value, then_bb, else_bb);

    // then statement codegen.
    common.builder.SetInsertPoint(then_bb);

    codegen_compound_statement(node.then_statement,
                               scope,
                               common,
                               retvar,
                               end_bb);

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(merge_bb);

    // else statement codegen.
    function->getBasicBlockList().push_back(else_bb);
    common.builder.SetInsertPoint(else_bb);

    if (node.else_statement) {
      codegen_compound_statement(*node.else_statement,
                                 scope,
                                 common,
                                 retvar,
                                 end_bb);
    }

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(merge_bb);

    function->getBasicBlockList().push_back(merge_bb);
    common.builder.SetInsertPoint(merge_bb);
  }

  void operator()(const ast::for_statement& node) const
  {
    if (node.init_expression) {
      auto init_value = boost::apply_visitor(expression_visitor{common, scope},
                                             *node.init_expression);

      if (!init_value) {
        throw std::runtime_error{format_error_message(
          common.file.string(),
          "failed to generate init expression in for statement")};
      }
    }

    auto function = common.builder.GetInsertBlock()->getParent();

    auto cond_bb
      = llvm::BasicBlock::Create(common.context, "for.cond", function);
    auto loop_bb = llvm::BasicBlock::Create(common.context, "for.loop");
    auto body_bb = llvm::BasicBlock::Create(common.context, "for.body");

    auto for_end_bb = llvm::BasicBlock::Create(common.context, "for.end");

    common.builder.CreateBr(cond_bb);
    common.builder.SetInsertPoint(cond_bb);

    if (node.cond_expression) {
      auto cond_value = boost::apply_visitor(expression_visitor{common, scope},
                                             *node.cond_expression);

      if (!cond_value) {
        throw std::runtime_error{format_error_message(
          common.file.string(),
          "failed to generate condition expression in for statement")};
      }

      cond_value = common.builder.CreateICmp(
        llvm::ICmpInst::ICMP_NE,
        cond_value,
        llvm::ConstantInt::get(
          common.typename_to_type(id::type_name::bool_).type,
          0));

      common.builder.CreateCondBr(cond_value, body_bb, for_end_bb);
    }
    else {
      // If condition is absent, unconditionally true.
      common.builder.CreateCondBr(
        llvm::ConstantInt::get(common.builder.getInt1Ty(), true),
        body_bb,
        for_end_bb);
    }

    function->getBasicBlockList().push_back(body_bb);
    common.builder.SetInsertPoint(body_bb);

    codegen_compound_statement(node.body, scope, common, retvar, end_bb);

    common.builder.CreateBr(loop_bb);

    function->getBasicBlockList().push_back(loop_bb);
    common.builder.SetInsertPoint(loop_bb);

    if (node.loop_expression) {
      auto loop_value = boost::apply_visitor(expression_visitor{common, scope},
                                             *node.loop_expression);

      if (!loop_value) {
        throw std::runtime_error{format_error_message(
          common.file.string(),
          "failed to generate loop expression in for statement")};
      }
    }

    common.builder.CreateBr(cond_bb);

    function->getBasicBlockList().push_back(for_end_bb);
    common.builder.SetInsertPoint(for_end_bb);
  }

private:
  codegen_common& common;

  symbol_table& scope;

  // Used to combine returns into one.
  llvm::AllocaInst* retvar;
  llvm::BasicBlock* end_bb;
};

void codegen_compound_statement(const ast::compound_statement& statements,
                                const symbol_table&            scope,
                                codegen_common&                common,
                                llvm::AllocaInst*              retvar,
                                llvm::BasicBlock*              end_bb)
{
  symbol_table new_scope = scope;

  for (auto&& statement : statements) {
    // If there is already a Terminator, the code generation of the main
    // body of the function is terminated on the spot.
    if (common.builder.GetInsertBlock()->getTerminator())
      break;

    boost::apply_visitor(statement_visitor{common, new_scope, retvar, end_bb},
                         statement);
  }
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct top_level_stmt_visitor : public boost::static_visitor<llvm::Function*> {
  top_level_stmt_visitor(codegen_common&                    common,
                         llvm::legacy::FunctionPassManager& function_pm)
    : common{common}
    , function_pm{function_pm}
  {
  }

  llvm::Function* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  // Function declaration
  llvm::Function* operator()(const ast::function_declare& node) const
  {
    std::vector<llvm::Type*> param_types(node.params.size());
    for (std::size_t i = 0, last = param_types.size(); i != last; ++i)
      param_types[i] = common.typename_to_type(node.params[i].type).type;

    auto function_type
      = llvm::FunctionType::get(common.typename_to_type(node.return_type).type,
                                param_types,
                                false);

    llvm::Function* function;
    if (!node.linkage) {
      // External linkage
      function = llvm::Function::Create(function_type,
                                        llvm::Function::ExternalLinkage,
                                        node.name,
                                        common.module);
    }
    else if (node.linkage == id::function_linkage::private_) {
      // Internal linkage
      function = llvm::Function::Create(function_type,
                                        llvm::Function::InternalLinkage,
                                        node.name,
                                        common.module);
    }

    // Set names for all arguments.
    std::size_t idx = 0;
    for (auto&& arg : function->args())
      arg.setName(node.params[idx++].name);

    return function;
  }

  // Function definition
  llvm::Function* operator()(const ast::function_define& node) const
  {
    auto function = common.module.getFunction(node.decl.name);

    if (!function)
      function = this->operator()(node.decl);

    if (!function) {
      throw std::runtime_error{format_error_message(
        common.file.string(),
        format("failed to create function %s", node.decl.name),
        true)};
    }

    symbol_table argument_values;

    auto entry_bb = llvm::BasicBlock::Create(common.context, "entry", function);
    common.builder.SetInsertPoint(entry_bb);

    for (auto& arg : function->args()) {
      const auto& param_info = node.decl.params[arg.getArgNo()];

      // Create an alloca for this variable.
      auto inst = create_entry_block_alloca(
        function,
        arg.getName().str(),
        common.typename_to_type(param_info.type).type);

      // Store the initial value into the alloca.
      common.builder.CreateStore(&arg, inst);

      // Add arguments to variable symbol table.
      if (!param_info.qualifier) {
        // consttant variable.
        argument_values.regist(arg.getName().str(), {inst, false});
      }
      else if (*param_info.qualifier == id::variable_qualifier::mutable_) {
        // mutable variable.
        argument_values.regist(arg.getName().str(), {inst, true});
      }
    }

    // Used to combine returns into one.
    auto end_bb = llvm::BasicBlock::Create(common.context, "end");
    auto retvar = node.decl.return_type == id::type_name::void_
                    ? nullptr
                    : create_entry_block_alloca(
                      function,
                      "retval",
                      common.typename_to_type(node.decl.return_type).type);

    codegen_compound_statement(node.body,
                               argument_values,
                               common,
                               retvar,
                               end_bb);

    // Return 0 if main function has no return.
    if (node.decl.name == "main"
        && !common.builder.GetInsertBlock()->getTerminator()) {
      common.builder.CreateStore(
        llvm::ConstantInt::get(common.builder.getInt32Ty(), 0),
        retvar);
      common.builder.CreateBr(end_bb);
    }

    // Automatically inserts a terminator if a function that returns void does
    // not have one.
    if (node.decl.return_type == id::type_name::void_
        && !common.builder.GetInsertBlock()->getTerminator()) {
      common.builder.CreateBr(end_bb);
    }

    // Return.
    function->getBasicBlockList().push_back(end_bb);
    common.builder.SetInsertPoint(end_bb);

    if (retvar) {
      auto retval
        = common.builder.CreateLoad(retvar->getAllocatedType(), retvar);
      common.builder.CreateRet(retval);
    }
    else {
      // Function that returns void.
      common.builder.CreateRet(nullptr);
    }

    std::string              em;
    llvm::raw_string_ostream os{em};
    if (llvm::verifyFunction(*function, &os)) {
      function->eraseFromParent();

      throw std::runtime_error{
        format_error_message(common.file.string(), os.str())};
    }

    function_pm.run(*function);

    return function;
  }

private:
  codegen_common& common;

  llvm::legacy::FunctionPassManager& function_pm;
};

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

codegen_common::codegen_common(const std::filesystem::path& file)
  : context{}
  , module{file.filename().string(), context}
  , builder{context}
  , file{file}
{
}

[[nodiscard]] integer_type
codegen_common::typename_to_type(const id::type_name type)
{
  switch (type) {
  case id::type_name::void_:
    return {builder.getVoidTy(), false};
  case id::type_name::i8:
    return {builder.getInt8Ty(), true};
  case id::type_name::u8:
    return {builder.getInt8Ty(), false};
  case id::type_name::i16:
    return {builder.getInt16Ty(), true};
  case id::type_name::u16:
    return {builder.getInt16Ty(), false};
  case id::type_name::i32:
    return {builder.getInt32Ty(), true};
  case id::type_name::u32:
    return {builder.getInt32Ty(), false};
  case id::type_name::i64:
    return {builder.getInt64Ty(), true};
  case id::type_name::u64:
    return {builder.getInt64Ty(), false};
  case id::type_name::bool_:
    // We will represent boolean by u8 instead of u1.
    // The reason for this is that unsigned is difficult to represent in llvm,
    // so with u1, true sometimes becomes -1! (My lack of technical skills ;_;)
    return {builder.getInt8Ty(), false};
  }

  BOOST_ASSERT(0);
}

[[nodiscard]] llvm::Value* codegen_common::i1_to_boolean(llvm::Value* value)
{
  auto as = typename_to_type(id::type_name::bool_);

  return llvm::CastInst::CreateIntegerCast(value,
                                           as.type,
                                           as.is_signed,
                                           "",
                                           builder.GetInsertBlock());
}

code_generator::code_generator(const ast::program&          ast,
                               const position_cache&        positions,
                               const std::filesystem::path& file,
                               const bool                   optimize)
  : common{file}
  , function_pm{&common.module}
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
    function_pm.add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    function_pm.add(llvm::createReassociatePass());
    // Eliminate Common SubExpressions.
    function_pm.add(llvm::createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    function_pm.add(llvm::createCFGSimplificationPass());
    // Promote allocas to registers.
    function_pm.add(llvm::createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    function_pm.add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    function_pm.add(llvm::createReassociatePass());
  }

  function_pm.doInitialization();

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

  common.module.setTargetTriple(target_triple);
  common.module.setDataLayout(target_machine->createDataLayout());

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

  common.module.print(os, nullptr);
}

void code_generator::write_object_code_to_file(const std::filesystem::path& out)
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

  pm.run(common.module);
  os.flush();
}

void code_generator::codegen()
{
  for (auto&& node : ast) {
    boost::apply_visitor(top_level_stmt_visitor{common, function_pm}, node);
  }
}

} // namespace miko::codegen
