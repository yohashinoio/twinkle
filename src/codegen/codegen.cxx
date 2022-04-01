/**
 * codegen.cxx
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/codegen.hxx>
#include <parse/id.hxx>
#include <utils/format.hxx>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace miko::codegen
{

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

struct variable_info {
  llvm::AllocaInst* inst;
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

  // Regist stands for register.
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
    for (const auto& r : named_values)
      std::cout << r.first << ' ';
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
    throw std::runtime_error{format_error_message(
      common.file.string(),
      "a function was executed that did not predict execution")};
  }

  // unsigned integer literals
  llvm::Value* operator()(const std::uint32_t node) const
  {
    return llvm::ConstantInt::get(common.builder.getInt32Ty(), node);
  }

  // signed integer literals
  llvm::Value* operator()(const std::int32_t node) const
  {
    return llvm::ConstantInt::getSigned(common.builder.getInt32Ty(), node);
  }

  // boolean literals
  llvm::Value* operator()(const bool node) const
  {
    return common.i1_to_boolean(
      llvm::ConstantInt::get(common.builder.getInt1Ty(), node));
  }

  // string literals
  llvm::Value* operator()(const ast::string_literal& node) const
  {
    return common.builder.CreateGlobalStringPtr(node.str);
  }

  llvm::Value* operator()(const ast::unary_op_expr& node) const
  {
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (!rhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right hand side")};
    }

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to 0-x
      return common.builder.CreateSub(
        llvm::ConstantInt::get(common.builder.getInt32Ty(), 0),
        rhs);
    }

    throw std::runtime_error{common.format_error(
      common.positions.position_of(node),
      format("unsupported unary operator '%s' detected", node.op))};
  }

  // Binary operator ast returns nullptr on failure because it cannot get a
  // position.
  llvm::Value* operator()(const ast::binary_op_expr& node) const
  {
    // Special case assignment because we don't want to emit the
    // left-hand-side as an expression.
    if (node.op == "=" || node.op == "+=" || node.op == "-=" || node.op == "*="
        || node.op == "/=" || node.op == "%=") {
      try {
        auto& lhs_node = boost::get<ast::variable_expr>(node.lhs);

        auto rhs = boost::apply_visitor(*this, node.rhs);

        if (!rhs) {
          throw std::runtime_error{
            common.format_error(common.positions.position_of(lhs_node),
                                "failed to generate right hand side")};
        }

        auto var_info = scope[lhs_node.name];

        if (!var_info) {
          // Unknown variable name.
          throw std::runtime_error{common.format_error(
            common.positions.position_of(lhs_node),
            format("unknown variable name '%s'", lhs_node.name))};
        }

        if (!var_info->is_mutable) {
          // Assignment of read-only variable.
          throw std::runtime_error{common.format_error(
            common.positions.position_of(lhs_node),
            format("assignment of read-only variable '%s'", lhs_node.name))};
        }

        if (node.op == "=")
          common.builder.CreateStore(rhs, var_info->inst);

        auto lhs = common.builder.CreateLoad(var_info->inst->getAllocatedType(),
                                             var_info->inst);

        if (node.op == "+=") {
          common.builder.CreateStore(common.builder.CreateAdd(lhs, rhs),
                                     var_info->inst);
        }

        if (node.op == "-=") {
          common.builder.CreateStore(common.builder.CreateSub(lhs, rhs),
                                     var_info->inst);
        }

        if (node.op == "*=") {
          common.builder.CreateStore(common.builder.CreateMul(lhs, rhs),
                                     var_info->inst);
        }

        if (node.op == "/=") {
          // TODO: unsigned
          common.builder.CreateStore(common.builder.CreateSDiv(lhs, rhs),
                                     var_info->inst);
        }

        if (node.op == "%=") {
          // TODO: unsigned
          common.builder.CreateStore(common.builder.CreateSRem(lhs, rhs),
                                     var_info->inst);
        }

        return common.builder.CreateLoad(var_info->inst->getAllocatedType(),
                                         var_info->inst);
      }
      catch (const boost::bad_get&) {
        // Left hand side was not a variable.
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "left hand side was not as variable",
                              false)};
      }
    }

    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate left hand side",
                            false)};
    }

    if (!rhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right hand side",
                            false)};
    }

    // addition
    if (node.op == "+")
      return common.builder.CreateAdd(lhs, rhs);
    // subtraction
    if (node.op == "-")
      return common.builder.CreateSub(lhs, rhs);

    // multiplication
    if (node.op == "*")
      return common.builder.CreateMul(lhs, rhs);
    // division
    if (node.op == "/") {
      // TODO: unsigned
      return common.builder.CreateSDiv(lhs, rhs);
    }
    if (node.op == "%") {
      // TODO: unsigned
      return common.builder.CreateSRem(lhs, rhs);
    }

    // equal
    if (node.op == "==") {
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhs, rhs));
    }
    // not equal
    if (node.op == "!=") {
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhs, rhs));
    }

    // less than
    if (node.op == "<") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SLT, lhs, rhs));
    }
    // greater than
    if (node.op == ">") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SGT, lhs, rhs));
    }
    // less or equal
    if (node.op == "<=") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SLE, lhs, rhs));
    }
    // greater or equal
    if (node.op == ">=") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SGE, lhs, rhs));
    }

    // Unsupported binary operators detected.
    throw std::runtime_error{common.format_error(
      common.positions.position_of(node),
      format("unsupported binary operator '%s' detected", node.op),
      false)};
  }

  llvm::Value* operator()(const ast::variable_expr& node) const
  {
    auto var_info = scope[node.name];

    if (!var_info) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("unknown variable '%s' referenced", node.name))};
    }

    return common.builder.CreateLoad(var_info->inst->getAllocatedType(),
                                     var_info->inst,
                                     node.name.c_str());
  }

  llvm::Value* operator()(const ast::function_call_expr& node) const
  {
    auto callee_function = common.module->getFunction(node.callee);

    if (!callee_function) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("unknown function '%s' referenced", node.callee))};
    }

    if (!callee_function->isVarArg()
        && callee_function->arg_size() != node.args.size()) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            format("incorrect arguments passed"))};
    }

    std::vector<llvm::Value*> args_value;
    for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
      args_value.push_back(boost::apply_visitor(*this, node.args[i]));

      if (!args_value.back()) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          format("argument set failed in call to function '%s'", node.callee))};
      }
    }

    return common.builder.CreateCall(callee_function, args_value);
  }

  llvm::Value* operator()(const ast::cast_expr& node) const
  {
    auto lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        "failed to generate left hand side of conversion operation")};
    }

    auto as = common.typename_to_type(node.as.id, node.as.is_ptr);

    if (!as) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "conversion to undefined type")};
    }

    return common.builder.CreateIntCast(lhs, as->type, as->is_signed);

    return lhs;
  }

  llvm::Value* operator()(const ast::address_of_expr& node) const
  {
    auto lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        "failed to generate left hand side of address of operation")};
    }

    return llvm::getPointerOperand(lhs);
  }

private:
  codegen_common& common;

  symbol_table& scope;
};

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

void codegen_statement(const ast::statement& statement,
                       const symbol_table&   scope,
                       codegen_common&       common,
                       llvm::AllocaInst*     retvar,
                       llvm::BasicBlock*     end_bb,
                       llvm::BasicBlock*     break_bb,
                       llvm::BasicBlock*     continue_bb);

struct statement_visitor : public boost::static_visitor<void> {
  statement_visitor(codegen_common&   common,
                    symbol_table&     scope,
                    llvm::AllocaInst* retvar,
                    llvm::BasicBlock* end_bb,
                    llvm::BasicBlock* break_bb,
                    llvm::BasicBlock* continue_bb)
    : common{common}
    , scope{scope}
    , retvar{retvar}
    , end_bb{end_bb}
    , break_bb{break_bb}
    , continue_bb{continue_bb}
  {
  }

  void operator()(ast::nil) const
  {
    // Empty statement, so not processed.
  }

  // Block
  void operator()(const ast::compound_statement& node) const
  {
    codegen_statement(node,
                      scope,
                      common,
                      retvar,
                      end_bb,
                      break_bb,
                      continue_bb);
  }

  void operator()(const ast::expression& node) const
  {
    if (!boost::apply_visitor(expression_visitor{common, scope}, node)) {
      throw std::runtime_error{
        format_error_message(common.file.string(),
                             "failed to generate expression statement")};
    }
  }

  void operator()(const ast::return_statement& node) const
  {
    if (node.rhs) {
      auto retval
        = boost::apply_visitor(expression_visitor{common, scope}, *node.rhs);

      if (!retval) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
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
        common.format_error(common.positions.position_of(node),
                            format("redefinition of '%s'", node.name))};
    }

    auto function = common.builder.GetInsertBlock()->getParent();

    auto type_info = common.typename_to_type(node.type.id, node.type.is_ptr);

    if (!type_info) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "variables of undefined type cannot be defined")};
    }

    auto inst = create_entry_block_alloca(function, node.name, type_info->type);

    if (node.initializer) {
      auto initializer = boost::apply_visitor(expression_visitor{common, scope},
                                              *node.initializer);

      if (!initializer) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          format("initialization of variable %s failed", node.name))};
      }

      common.builder.CreateStore(initializer, inst);
    }

    if (!node.qualifier) {
      // consttant variable.
      scope.regist(node.name, {inst, false, type_info->is_signed});
    }
    else if (*node.qualifier == id::variable_qualifier::mutable_) {
      // mutable variable.
      scope.regist(node.name, {inst, true, type_info->is_signed});
    }
  }

  void operator()(const ast::if_statement& node) const
  {
    auto condition_value
      = boost::apply_visitor(expression_visitor{common, scope}, node.condition);

    if (!condition_value) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "invalid condition in if statement")};
    }

    // Convert condition to a bool by comparing non-equal to 0.
    condition_value = common.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      condition_value,
      llvm::ConstantInt::get(
        common.typename_to_type(id::type_name::bool_)->type,
        0));

    auto function = common.builder.GetInsertBlock()->getParent();

    auto then_bb = llvm::BasicBlock::Create(*common.context, "", function);
    auto else_bb = llvm::BasicBlock::Create(*common.context);

    auto merge_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateCondBr(condition_value, then_bb, else_bb);

    // then statement codegen.
    common.builder.SetInsertPoint(then_bb);

    codegen_statement(node.then_statement,
                      scope,
                      common,
                      retvar,
                      end_bb,
                      break_bb,
                      continue_bb);

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(merge_bb);

    // else statement codegen.
    function->getBasicBlockList().push_back(else_bb);
    common.builder.SetInsertPoint(else_bb);

    if (node.else_statement) {
      codegen_statement(*node.else_statement,
                        scope,
                        common,
                        retvar,
                        end_bb,
                        break_bb,
                        continue_bb);
    }

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(merge_bb);

    function->getBasicBlockList().push_back(merge_bb);
    common.builder.SetInsertPoint(merge_bb);
  }

  void operator()(const ast::loop_statement& node) const
  {
    auto function = common.builder.GetInsertBlock()->getParent();

    auto body_bb = llvm::BasicBlock::Create(*common.context, "", function);

    auto loop_end_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateBr(body_bb);
    common.builder.SetInsertPoint(body_bb);

    codegen_statement(node.body,
                      scope,
                      common,
                      retvar,
                      end_bb,
                      loop_end_bb,
                      body_bb);

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(body_bb);

    function->getBasicBlockList().push_back(loop_end_bb);
    common.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::while_statement& node) const
  {
    auto function = common.builder.GetInsertBlock()->getParent();

    auto cond_bb = llvm::BasicBlock::Create(*common.context, "", function);
    auto body_bb = llvm::BasicBlock::Create(*common.context);

    auto loop_end_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateBr(cond_bb);
    common.builder.SetInsertPoint(cond_bb);

    auto cond_value
      = boost::apply_visitor(expression_visitor{common, scope}, node.cond_expr);

    if (!cond_value) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        "failed to generate condition expression in for statement")};
    }

    cond_value = common.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value,
      llvm::ConstantInt::get(
        common.typename_to_type(id::type_name::bool_)->type,
        0));

    common.builder.CreateCondBr(cond_value, body_bb, loop_end_bb);

    function->getBasicBlockList().push_back(body_bb);
    common.builder.SetInsertPoint(body_bb);

    codegen_statement(node.body,
                      scope,
                      common,
                      retvar,
                      end_bb,
                      loop_end_bb,
                      cond_bb);

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(cond_bb);

    function->getBasicBlockList().push_back(loop_end_bb);
    common.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::for_statement& node) const
  {
    if (node.init_expr) {
      auto init_value = boost::apply_visitor(expression_visitor{common, scope},
                                             *node.init_expr);

      if (!init_value) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          "failed to generate init expression in for statement")};
      }
    }

    auto function = common.builder.GetInsertBlock()->getParent();

    auto cond_bb = llvm::BasicBlock::Create(*common.context, "", function);
    auto loop_bb = llvm::BasicBlock::Create(*common.context);
    auto body_bb = llvm::BasicBlock::Create(*common.context);

    auto loop_end_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateBr(cond_bb);
    common.builder.SetInsertPoint(cond_bb);

    if (node.cond_expr) {
      auto cond_value = boost::apply_visitor(expression_visitor{common, scope},
                                             *node.cond_expr);

      if (!cond_value) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          "failed to generate condition expression in for statement")};
      }

      cond_value = common.builder.CreateICmp(
        llvm::ICmpInst::ICMP_NE,
        cond_value,
        llvm::ConstantInt::get(
          common.typename_to_type(id::type_name::bool_)->type,
          0));

      common.builder.CreateCondBr(cond_value, body_bb, loop_end_bb);
    }
    else {
      // If condition is absent, unconditionally true.
      common.builder.CreateCondBr(
        llvm::ConstantInt::get(common.builder.getInt1Ty(), true),
        body_bb,
        loop_end_bb);
    }

    function->getBasicBlockList().push_back(body_bb);
    common.builder.SetInsertPoint(body_bb);

    codegen_statement(node.body,
                      scope,
                      common,
                      retvar,
                      end_bb,
                      loop_end_bb,
                      loop_bb);

    if (!common.builder.GetInsertBlock()->getTerminator())
      common.builder.CreateBr(loop_bb);

    function->getBasicBlockList().push_back(loop_bb);
    common.builder.SetInsertPoint(loop_bb);

    if (node.loop_expr) {
      auto loop_value = boost::apply_visitor(expression_visitor{common, scope},
                                             *node.loop_expr);

      if (!loop_value) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          "failed to generate loop expression in for statement")};
      }
    }

    common.builder.CreateBr(cond_bb);

    function->getBasicBlockList().push_back(loop_end_bb);
    common.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(ast::break_statement) const
  {
    // Whether in a loop.
    if (break_bb)
      common.builder.CreateBr(break_bb);
  }

  void operator()(ast::continue_statement) const
  {
    // Whether in a loop.
    if (continue_bb)
      common.builder.CreateBr(continue_bb);
  }

private:
  codegen_common& common;

  symbol_table& scope;

  // Used to combine returns into one.
  llvm::AllocaInst* retvar;
  llvm::BasicBlock* end_bb;

  // If not in loop, nullptr.
  llvm::BasicBlock* break_bb; // Basic block transitioned by break statement.
  llvm::BasicBlock*
    continue_bb; // Basic block transitioned by continue statement.
};

void codegen_statement(const ast::statement& statement,
                       const symbol_table&   scope,
                       codegen_common&       common,
                       llvm::AllocaInst*     retvar,
                       llvm::BasicBlock*     end_bb,
                       llvm::BasicBlock*     break_bb,
                       llvm::BasicBlock*     continue_bb)
{
  symbol_table new_scope = scope;

  // Compound statement
  if (statement.type() == typeid(ast::compound_statement)) {
    auto statements = boost::get<ast::compound_statement>(statement);

    for (const auto& r : statements) {
      boost::apply_visitor(statement_visitor{common,
                                             new_scope,
                                             retvar,
                                             end_bb,
                                             break_bb,
                                             continue_bb},
                           r);

      // If a terminator is present, subsequent code generation is terminated.
      if (common.builder.GetInsertBlock()->getTerminator())
        break;
    }

    return;
  }

  // Other than compound statement
  boost::apply_visitor(
    statement_visitor{common, new_scope, retvar, end_bb, break_bb, continue_bb},
    statement);
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct top_level_stmt_visitor : public boost::static_visitor<llvm::Function*> {
  top_level_stmt_visitor(codegen_common&                    common,
                         llvm::legacy::FunctionPassManager& fpm)
    : common{common}
    , fpm{fpm}
  {
  }

  llvm::Function* operator()(ast::nil) const
  {
    throw std::runtime_error{format_error_message(
      common.file.string(),
      "a function was executed that did not predict execution")};
  }

  // Function declaration
  llvm::Function* operator()(const ast::function_declare& node) const
  {
    auto&& ps = *node.params;

    if (ps.size() && ps.at(0).is_vararg) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "requires a named argument before '...'")};
    }

    bool is_vararg = false;
    for (const auto& r : ps) {
      if (r.is_vararg) {
        if (is_vararg) {
          throw std::runtime_error{
            common.format_error(common.positions.position_of(node),
                                "cannot have multiple variable arguments")};
        }
        else
          is_vararg = true;
      }
    }

    const auto named_params_length
      = is_vararg ? node.params.length() - 1 : node.params.length();
    std::vector<llvm::Type*> param_types(named_params_length);

    for (std::size_t i = 0; i != named_params_length; ++i) {
      const auto& param_info = node.params[i].type;
      param_types.at(i)
        = common.typename_to_type(param_info.id, param_info.is_ptr)->type;
    }

    auto function_type = llvm::FunctionType::get(
      common.typename_to_type(node.return_type.id)->type,
      param_types,
      is_vararg);

    llvm::Function* function;
    if (!node.linkage) {
      // External linkage
      function = llvm::Function::Create(function_type,
                                        llvm::Function::ExternalLinkage,
                                        node.name,
                                        *common.module);
    }
    else if (node.linkage == id::function_linkage::private_) {
      // Internal linkage
      function = llvm::Function::Create(function_type,
                                        llvm::Function::InternalLinkage,
                                        node.name,
                                        *common.module);
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
    auto function = common.module->getFunction(node.decl.name);

    if (!function)
      function = this->operator()(node.decl);

    if (!function) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("failed to create function %s", node.decl.name))};
    }

    symbol_table argument_values;

    auto entry_bb = llvm::BasicBlock::Create(*common.context, "", function);
    common.builder.SetInsertPoint(entry_bb);

    for (auto& arg : function->args()) {
      const auto& param_node = node.decl.params[arg.getArgNo()];

      auto arg_type
        = common.typename_to_type(param_node.type.id, param_node.type.is_ptr);

      if (!arg_type) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          "arguments of undefined types cannot be declared")};
      }

      // Create an alloca for this variable.
      auto inst = create_entry_block_alloca(function,
                                            arg.getName().str(),
                                            arg_type->type);

      // Store the initial value into the alloca.
      common.builder.CreateStore(&arg, inst);

      // Add arguments to variable symbol table.
      if (!param_node.qualifier) {
        // consttant variable.
        argument_values.regist(arg.getName().str(), {inst, false});
      }
      else if (*param_node.qualifier == id::variable_qualifier::mutable_) {
        // mutable variable.
        argument_values.regist(arg.getName().str(), {inst, true});
      }
    }

    auto return_type = common.typename_to_type(node.decl.return_type.id,
                                               node.decl.return_type.is_ptr);

    if (!return_type) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "return type cannot be an undefined type")};
    }

    // Used to combine returns into one.
    auto end_bb = llvm::BasicBlock::Create(*common.context);
    auto retvar
      = node.decl.return_type.id == id::type_name::void_
          ? nullptr
          : create_entry_block_alloca(function, "", return_type->type);

    codegen_statement(node.body,
                      argument_values,
                      common,
                      retvar,
                      end_bb,
                      nullptr,
                      nullptr);

    // If there is no return, returns undef.
    if (!common.builder.GetInsertBlock()->getTerminator()
        && node.decl.return_type.id != id::type_name::void_) {
      // Return 0 specially for main.
      if (node.decl.name == "main") {
        common.builder.CreateStore(
          llvm::ConstantInt::getSigned(function->getReturnType(), 0),
          retvar);
        common.builder.CreateBr(end_bb);
      }
      else {
        common.builder.CreateStore(
          llvm::UndefValue::get(function->getReturnType()),
          retvar);
        common.builder.CreateBr(end_bb);
      }
    }

    // Inserts a terminator if the function returning void does not have one.
    if (node.decl.return_type.id == id::type_name::void_
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
        common.format_error(common.positions.position_of(node), os.str())};
    }

    fpm.run(*function);

    return function;
  }

private:
  codegen_common& common;

  llvm::legacy::FunctionPassManager& fpm;
};

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

codegen_common::codegen_common(const std::filesystem::path& file,
                               const position_cache&        positions)
  : context{std::make_unique<llvm::LLVMContext>()}
  , module{std::make_unique<llvm::Module>(file.filename().string(), *context)}
  , builder{*context}
  , file{file}
  , positions{positions}
{
}

[[nodiscard]] std::optional<llvm_type_info>
codegen_common::typename_to_type(const id::type_name type, const bool is_ptr)
{
  llvm_type_info tmp;

  switch (type) {
  case id::type_name::void_:
    tmp = {builder.getVoidTy(), false};
    break;
  case id::type_name::i8:
    tmp = {builder.getInt8Ty(), true};
    break;
  case id::type_name::u8:
    tmp = {builder.getInt8Ty(), false};
    break;
  case id::type_name::i16:
    tmp = {builder.getInt16Ty(), true};
    break;
  case id::type_name::u16:
    tmp = {builder.getInt16Ty(), false};
    break;
  case id::type_name::i32:
    tmp = {builder.getInt32Ty(), true};
    break;
  case id::type_name::u32:
    tmp = {builder.getInt32Ty(), false};
    break;
  case id::type_name::i64:
    tmp = {builder.getInt64Ty(), true};
    break;
  case id::type_name::u64:
    tmp = {builder.getInt64Ty(), false};
    break;
  case id::type_name::bool_:
    // We will represent boolean by u8 instead of i1.
    tmp = {builder.getInt8Ty(), false};
    break;
  default:
    return std::nullopt;
  }

  if (is_ptr) {
    // Get pointer type.
    tmp.type = llvm::PointerType::getUnqual(tmp.type);
  }

  return tmp;
}

[[nodiscard]] llvm::Value* codegen_common::i1_to_boolean(llvm::Value* value)
{
  auto as = typename_to_type(id::type_name::bool_);

  if (!as)
    BOOST_ASSERT(0);

  return llvm::CastInst::CreateIntegerCast(value,
                                           as->type,
                                           as->is_signed,
                                           "",
                                           builder.GetInsertBlock());
}

[[nodiscard]] std::string codegen_common::format_error(
  const boost::iterator_range<input_iterator_type> pos,
  const std::string_view                           message,
  const bool                                       with_code)
{
  // Calculate line numbers.
  std::size_t rows = 0;
  for (auto iter = pos.begin();; --iter) {
    if (*iter == '\n')
      ++rows;

    if (iter == positions.first()) {
      ++rows;
      break;
    }
  }

  std::ostringstream ss;

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    ss << "In file " << file.string() << ", line " << rows << ":\n"
       << COLOR_RED "error: " COLOR_DEFAULT << message << '\n';
  }
#else
  ss << "In file " << file.string() << ", line " << line << ":\n"
     << "error: " << message << '\n';
#endif

  if (with_code) {
    std::for_each(pos.begin(), pos.end(), [&](auto&& ch) { ss << ch; });

    // TODO:
    // ss << "\n^_";
  }

  return ss.str();
}

code_generator::code_generator(const std::string_view       program_name,
                               const ast::program&          ast,
                               const position_cache&        positions,
                               const std::filesystem::path& file,
                               const bool                   optimize)
  : program_name{program_name}
  , common{file, positions}
  , fpm{common.module.get()}
  , ast{ast}
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

  // Set target triple and data layout to module
  const auto target_triple = llvm::sys::getDefaultTargetTriple();

  std::string target_triple_error;
  auto        target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_error);

  if (!target) {
    throw std::runtime_error{
      format_error_message(program_name,
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

  common.module->setTargetTriple(target_triple);
  common.module->setDataLayout(target_machine->createDataLayout());

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
      program_name,
      format("%s: %s", out.string(), ostream_ec.message()))};
  }

  common.module->print(os, nullptr);
}

void code_generator::write_object_code_to_file(const std::filesystem::path& out)
{
  std::error_code      ostream_ec;
  llvm::raw_fd_ostream os{out.string(),
                          ostream_ec,
                          llvm::sys::fs::OpenFlags::OF_None};
  if (ostream_ec) {
    throw std::runtime_error{format_error_message(
      program_name,
      format("%s: %s\n", out.string(), ostream_ec.message()))};
  }

  llvm::legacy::PassManager pm;
  if (target_machine->addPassesToEmitFile(pm,
                                          os,
                                          nullptr,
                                          llvm::CGFT_ObjectFile)) {
    throw std::runtime_error{
      format_error_message(program_name,
                           "targetMachine can't emit a file of this types",
                           true)};
  }

  pm.run(*common.module);
  os.flush();
}

// Returns the return value from the main function.
[[nodiscard]] int code_generator::jit_compile()
{
  auto jit_expected = jit::jit_compiler::create();
  if (auto err = jit_expected.takeError()) {
    // Error
    throw std::runtime_error{
      format_error_message(common.file.string(),
                           llvm::toString(std::move(err)),
                           true)};
  }

  auto jit = std::move(*jit_expected);

  if (auto err = jit->add_module(
        {std::move(common.module), std::move(common.context)})) {
    // Error
    throw std::runtime_error{
      format_error_message(common.file.string(),
                           llvm::toString(std::move(err)))};
  }

  auto symbol_expected = jit->lookup("main");
  if (auto err = symbol_expected.takeError()) {
    // Error
    throw std::runtime_error{
      format_error_message(common.file.string(),
                           "Symbol main could not be found")};
  }

  auto symbol = *symbol_expected;
  auto main_addr
    = reinterpret_cast<int (*)(/* TODO: command line arguments */)>(
      symbol.getAddress());

  // Run main
  return main_addr();
}

void code_generator::codegen()
{
  for (const auto& node : ast)
    boost::apply_visitor(top_level_stmt_visitor{common, fpm}, node);
}

} // namespace miko::codegen
