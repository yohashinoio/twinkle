/**
 * codegen.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/codegen.hpp>
#include <utils/type.hpp>
#include <utils/format.hpp>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace miko::codegen
{

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

struct VariableInfo {
  llvm::AllocaInst* inst;
  bool              is_mutable;
  bool              is_signed;
};

struct SymbolTable {
  [[nodiscard]] std::optional<VariableInfo>
  operator[](const std::string& name) const noexcept
  try {
    return named_values.at(name);
  }
  catch (const std::out_of_range&) {
    return std::nullopt;
  }

  // Regist stands for register.
  void regist(const std::string& name, VariableInfo info)
  {
    named_values.insert({name, info});
  }

  // Returns true if the variable is already registered, false otherwise.
  bool exists(const std::string& name)
  {
    return named_values.contains(name);
  }

  // For debug.
  void print_table() const
  {
    for (const auto& r : named_values)
      std::cout << r.first << ' ';
    std::endl(std::cout);
  }

private:
  std::unordered_map<std::string, VariableInfo> named_values;
};

struct SignTable {
  [[nodiscard]] std::optional<bool>
  operator[](llvm::Value* value) const noexcept
  try {
    return sign_table.at(value);
  }
  catch (const std::out_of_range&) {
    return std::nullopt;
  }

  // Regist stands for register.
  void regist(llvm::Value* value, bool is_signed)
  {
    sign_table.insert({value, is_signed});
  }

  // Returns true if the variable is already registered, false otherwise.
  bool exists(llvm::Value* value)
  {
    return sign_table.contains(value);
  }

  // For debug.
  void print_table() const
  {
    for (const auto& r : sign_table)
      std::cout << r.first << ' ';
    std::endl(std::cout);
  }

private:
  std::unordered_map<llvm::Value*, bool> sign_table;
};

// Create an alloca instruction in the entry block of
// the function.
// This is used for variables etc.
[[nodiscard]] llvm::AllocaInst*
create_entry_block_alloca(llvm::Function*    func,
                          const std::string& var_name,
                          llvm::Type*        type)
{
  return llvm::IRBuilder<>{&func->getEntryBlock(),
                           func->getEntryBlock().begin()}
    .CreateAlloca(type, nullptr, var_name);
}

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct ExprVisitor : public boost::static_visitor<llvm::Value*> {
  ExprVisitor(CodegenContext& common, SymbolTable& scope)
    : common{common}
    , scope{scope}
  {
  }

  [[nodiscard]] llvm::Value* operator()(ast::Nil) const
  {
    BOOST_ASSERT(0);
    return nullptr;
  }

  // 32bit unsigned integer literals.
  [[nodiscard]] llvm::Value* operator()(const std::uint32_t node) const
  {
    return llvm::ConstantInt::get(common.builder.getInt32Ty(), node);
  }

  // 32bit signed integer literals.
  [[nodiscard]] llvm::Value* operator()(const std::int32_t node) const
  {
    return llvm::ConstantInt::getSigned(common.builder.getInt32Ty(), node);
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] llvm::Value* operator()(const std::uint64_t node) const
  {
    return llvm::ConstantInt::get(common.builder.getInt64Ty(), node);
  }

  // 64bit signed integer literals.
  [[nodiscard]] llvm::Value* operator()(const std::int64_t node) const
  {
    return llvm::ConstantInt::getSigned(common.builder.getInt64Ty(), node);
  }

  // Boolean literals.
  [[nodiscard]] llvm::Value* operator()(const bool node) const
  {
    return common.i1_to_boolean(
      llvm::ConstantInt::get(common.builder.getInt1Ty(), node));
  }

  [[nodiscard]] llvm::Value* operator()(const ast::StringLiteral& node) const
  {
    return common.builder.CreateGlobalStringPtr(node.str);
  }

  [[nodiscard]] llvm::Value* operator()(const ast::CharLiteral& node) const
  {
    return llvm::ConstantInt::get(common.builder.getInt8Ty(), node.ch);
  }

  [[nodiscard]] llvm::Value* operator()(const ast::UnaryOp& node) const
  {
    auto const rhs = boost::apply_visitor(*this, node.rhs);

    if (!rhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side")};
    }

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to (0 - x).
      return common.builder.CreateSub(
        llvm::ConstantInt::get(common.builder.getInt32Ty(), 0),
        rhs);
    }

    throw std::runtime_error{
      common.format_error(common.positions.position_of(node),
                          format("unknown operator '%s' detected", node.op))};
  }

  [[nodiscard]] llvm::Value* operator()(const ast::BinOp& node) const
  {
    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate left-hand side",
                            false)};
    }

    if (!rhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side",
                            false)};
    }

    // Implicit conversions.
    if (const auto lhs_bitwidth = lhs->getType()->getIntegerBitWidth(),
        rhs_bitwidth            = rhs->getType()->getIntegerBitWidth();
        lhs_bitwidth != rhs_bitwidth) {
      const auto max_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

      const auto as = common.builder.getIntNTy(max_bitwidth);

      if (lhs_bitwidth == max_bitwidth)
        rhs = common.builder.CreateIntCast(rhs, as, true); // TODO: sign
      else
        lhs = common.builder.CreateIntCast(lhs, as, true); // TODO: sign
    }

    if (lhs->getType() != rhs->getType()) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        "both operands to a binary operator are not of the same type",
        false)};
    }

    // Addition.
    if (node.op == "+")
      return common.builder.CreateAdd(lhs, rhs);

    // Subtraction.
    if (node.op == "-")
      return common.builder.CreateSub(lhs, rhs);

    // Multiplication.
    if (node.op == "*")
      return common.builder.CreateMul(lhs, rhs);

    // Division.
    if (node.op == "/") {
      // TODO: unsigned
      return common.builder.CreateSDiv(lhs, rhs);
    }

    // Modulo.
    if (node.op == "%") {
      // TODO: unsigned
      return common.builder.CreateSRem(lhs, rhs);
    }

    // Equal.
    if (node.op == "==") {
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhs, rhs));
    }

    // Not equal.
    if (node.op == "!=") {
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhs, rhs));
    }

    // Less than.
    if (node.op == "<") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SLT, lhs, rhs));
    }

    // Greater than.
    if (node.op == ">") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SGT, lhs, rhs));
    }

    // Less or equal.
    if (node.op == "<=") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SLE, lhs, rhs));
    }

    // Greater or equal.
    if (node.op == ">=") {
      // TODO: unsigned
      return common.i1_to_boolean(
        common.builder.CreateICmp(llvm::ICmpInst::ICMP_SGE, lhs, rhs));
    }

    // Unsupported binary operators detected.
    throw std::runtime_error{
      common.format_error(common.positions.position_of(node),
                          format("unknown operator '%s' detected", node.op),
                          false)};
  }

  [[nodiscard]] llvm::Value* operator()(const ast::VariableRef& node) const
  {
    auto var_info = scope[node.name];

    if (!var_info) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("unknown variable '%s' referenced", node.name))};
    }

    return common.builder.CreateLoad(var_info->inst->getAllocatedType(),
                                     var_info->inst);
  }

  [[nodiscard]] llvm::Value* operator()(const ast::FunctionCall& node) const
  {
    auto const callee_func = common.module->getFunction(node.callee);

    if (!callee_func) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("unknown function '%s' referenced", node.callee))};
    }

    if (!callee_func->isVarArg()
        && callee_func->arg_size() != node.args.size()) {
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
          format("argument set failed in call to the function '%s'",
                 node.callee))};
      }
    }

    // Verify arguments
    for (std::size_t idx = 0; auto&& arg : callee_func->args()) {
      if (args_value[idx++]->getType() != arg.getType()) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          format("incompatible type for argument %d of '%s'",
                 idx + 1,
                 node.callee))};
      }
    }

    return common.builder.CreateCall(callee_func, args_value);
  }

  [[nodiscard]] llvm::Value* operator()(const ast::Conversion& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate left-hand side")};
    }

    const auto as = common.kind_to_type(*node.as);

    return common.builder.CreateIntCast(lhs, as.type, as.is_signed);
  }

  [[nodiscard]] llvm::Value* operator()(const ast::AddressOf& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side")};
    }

    return llvm::getPointerOperand(lhs);
  }

  [[nodiscard]] llvm::Value* operator()(const ast::Indirection& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side")};
    }

    auto const lhs_type = lhs->getType();

    if (!lhs_type->isPointerTy()) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "unary '*' requires pointer operand")};
    }

    return common.builder.CreateLoad(lhs_type->getPointerElementType(), lhs);
  }

private:
  CodegenContext& common;

  SymbolTable& scope;
};

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

void codegen_statement(const ast::Stmt&   statement,
                       const SymbolTable& scope,
                       CodegenContext&    common,
                       llvm::AllocaInst*  retvar,
                       llvm::BasicBlock*  end_bb,
                       llvm::BasicBlock*  break_bb,
                       llvm::BasicBlock*  continue_bb);

struct StmtVisitor : public boost::static_visitor<void> {
  StmtVisitor(CodegenContext&   common,
              SymbolTable&      scope,
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

  void operator()(ast::Nil) const
  {
    // Empty statement, so not processed.
  }

  void operator()(const ast::CompoundStmt& node) const
  {
    codegen_statement(node,
                      scope,
                      common,
                      retvar,
                      end_bb,
                      break_bb,
                      continue_bb);
  }

  void operator()(const ast::Expr& node) const
  {
    if (!boost::apply_visitor(ExprVisitor{common, scope}, node)) {
      throw std::runtime_error{
        format_error_message(common.file.string(),
                             "failed to generate expression statement")};
    }
  }

  void operator()(const ast::Return& node) const
  {
    if (node.rhs) {
      auto const retval
        = boost::apply_visitor(ExprVisitor{common, scope}, *node.rhs);

      if (common.builder.GetInsertBlock()->getParent()->getReturnType()
          != retval->getType()) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "incompatible type for result type")};
      }

      if (!retval) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "failed to generate return value")};
      }

      common.builder.CreateStore(retval, retvar);
    }

    common.builder.CreateBr(end_bb);
  }

  void operator()(const ast::VariableDef& node) const
  {
    if (!node.type.has_value() && !node.initializer.has_value()) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "type inference requires an initializer")};
    }

    if (scope.exists(node.name)) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            format("redefinition of '%s'", node.name))};
    }

    auto const func = common.builder.GetInsertBlock()->getParent();

    const auto regist
      = [&](llvm::AllocaInst* const inst, const bool is_signed) {
          if (!node.qualifier) {
            // Consttant variable.
            scope.regist(node.name, {inst, false, is_signed});
          }
          else if (*node.qualifier == VariableQual::mutable_) {
            // Mutable variable.
            scope.regist(node.name, {inst, true, is_signed});
          }
        };

    if (node.type) {
      const auto type_info = common.kind_to_type(**node.type);

      auto const inst
        = create_entry_block_alloca(func, node.name, type_info.type);

      if (node.initializer) {
        auto const init_value
          = boost::apply_visitor(ExprVisitor{common, scope}, *node.initializer);

        if (!init_value) {
          throw std::runtime_error{common.format_error(
            common.positions.position_of(node),
            format("failed to generate initializer for '%s'", node.name))};
        }

        if (type_info.type != init_value->getType()) {
          throw std::runtime_error{common.format_error(
            common.positions.position_of(node),
            "Initializer type and variable type are different")};
        }

        common.builder.CreateStore(init_value, inst);
      }

      regist(inst, type_info.is_signed);
    }
    else {
      // Type inference.
      auto const init_value
        = boost::apply_visitor(ExprVisitor{common, scope}, *node.initializer);

      if (!init_value) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          format("failed to generate initializer for '%s'", node.name))};
      }

      auto const inst
        = create_entry_block_alloca(func, node.name, init_value->getType());

      common.builder.CreateStore(init_value, inst);

      regist(inst, true); // TODO: sign
    }
  }

  void operator()(const ast::Assignment& node) const
  {
    if (node.op == "=" || node.op == "+=" || node.op == "-=" || node.op == "*="
        || node.op == "/=" || node.op == "%=") {
      llvm::Value* lhs;

      if (node.lhs.type() == typeid(ast::VariableRef)) {
        auto& lhs_node = boost::get<ast::VariableRef>(node.lhs);

        auto var_info = scope[lhs_node.name];

        if (!var_info) {
          // Unknown variable name.
          throw std::runtime_error{common.format_error(
            common.positions.position_of(node),
            format("unknown variable name '%s'", lhs_node.name))};
        }

        if (!var_info->is_mutable) {
          // Assignment of read-only variable.
          throw std::runtime_error{common.format_error(
            common.positions.position_of(node),
            format("assignment of read-only variable '%s'", lhs_node.name))};
        }

        lhs = var_info->inst;
      }
      else if (node.lhs.type() == typeid(ast::Indirection)) {
        auto const lhs_node = boost::get<ast::Indirection>(node.lhs);

        lhs = boost::apply_visitor(ExprVisitor{common, scope}, lhs_node.lhs);
      }
      else
        lhs = boost::apply_visitor(ExprVisitor{common, scope}, node.lhs);

      if (!lhs) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "failed to generate left-hand side")};
      }

      if (!lhs->getType()->isPointerTy()) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "left-hand side requires assignable")};
      }

      auto const rhs
        = boost::apply_visitor(ExprVisitor{common, scope}, node.rhs);

      if (!rhs) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "failed to generate right-hand side")};
      }

      if (lhs->getType()->getPointerElementType() != rhs->getType()) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          "both operands to a binary operator are not of the same type")};
      }

      auto const lhs_value
        = common.builder.CreateLoad(lhs->getType()->getPointerElementType(),
                                    lhs);

      // Direct assignment.
      if (node.op == "=")
        common.builder.CreateStore(rhs, lhs);

      // Addition assignment.
      if (node.op == "+=") {
        common.builder.CreateStore(common.builder.CreateAdd(lhs_value, rhs),
                                   lhs);
      }

      // Subtraction assignment.
      if (node.op == "-=") {
        common.builder.CreateStore(common.builder.CreateSub(lhs_value, rhs),
                                   lhs);
      }

      // Multiplication assignment.
      if (node.op == "*=") {
        common.builder.CreateStore(common.builder.CreateMul(lhs_value, rhs),
                                   lhs);
      }

      // Division assignment.
      if (node.op == "/=") {
        auto const assign_value = true // TODO:
                                    ? common.builder.CreateSDiv(lhs_value, rhs)
                                    : common.builder.CreateUDiv(lhs_value, rhs);

        common.builder.CreateStore(assign_value, lhs);
      }

      // Modulo assignment.
      if (node.op == "%=") {
        auto const assign_value = true /* TODO: var_info->is_signed */
                                    ? common.builder.CreateSRem(lhs_value, rhs)
                                    : common.builder.CreateURem(lhs_value, rhs);

        common.builder.CreateStore(assign_value, lhs);
      }
    }
  }

  void operator()(const ast::If& node) const
  {
    auto const cond_value
      = boost::apply_visitor(ExprVisitor{common, scope}, node.condition);

    if (!cond_value) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "invalid condition in if statement")};
    }

    // Convert condition to a bool by comparing non-equal to 0.
    auto const cond = common.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value,
      llvm::ConstantInt::get(
        common.kind_to_type(BuiltinType{BuiltinTypeKind::bool_}).type,
        0));

    auto const func = common.builder.GetInsertBlock()->getParent();

    auto const then_bb = llvm::BasicBlock::Create(*common.context, "", func);
    auto const else_bb = llvm::BasicBlock::Create(*common.context);

    auto const merge_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateCondBr(cond, then_bb, else_bb);

    // Then statement codegen.
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

    // Else statement codegen.
    func->getBasicBlockList().push_back(else_bb);
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

    func->getBasicBlockList().push_back(merge_bb);
    common.builder.SetInsertPoint(merge_bb);
  }

  void operator()(const ast::Loop& node) const
  {
    auto const func = common.builder.GetInsertBlock()->getParent();

    auto const body_bb = llvm::BasicBlock::Create(*common.context, "", func);

    auto const loop_end_bb = llvm::BasicBlock::Create(*common.context);

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

    func->getBasicBlockList().push_back(loop_end_bb);
    common.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::While& node) const
  {
    auto const func = common.builder.GetInsertBlock()->getParent();

    auto const cond_bb = llvm::BasicBlock::Create(*common.context, "", func);
    auto const body_bb = llvm::BasicBlock::Create(*common.context);

    auto const loop_end_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateBr(cond_bb);
    common.builder.SetInsertPoint(cond_bb);

    auto const cond_value
      = boost::apply_visitor(ExprVisitor{common, scope}, node.cond_expr);

    if (!cond_value) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate condition expression")};
    }

    auto const cond = common.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value,
      llvm::ConstantInt::get(
        common.kind_to_type(BuiltinType{BuiltinTypeKind::bool_}).type,
        0));

    common.builder.CreateCondBr(cond, body_bb, loop_end_bb);

    func->getBasicBlockList().push_back(body_bb);
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

    func->getBasicBlockList().push_back(loop_end_bb);
    common.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::For& node) const
  {
    if (node.init_stmt)
      boost::apply_visitor(*this, *node.init_stmt);

    auto const func = common.builder.GetInsertBlock()->getParent();

    auto const cond_bb = llvm::BasicBlock::Create(*common.context, "", func);
    auto const loop_bb = llvm::BasicBlock::Create(*common.context);
    auto const body_bb = llvm::BasicBlock::Create(*common.context);

    auto const loop_end_bb = llvm::BasicBlock::Create(*common.context);

    common.builder.CreateBr(cond_bb);
    common.builder.SetInsertPoint(cond_bb);

    if (node.cond_expr) {
      auto const cond_value
        = boost::apply_visitor(ExprVisitor{common, scope}, *node.cond_expr);

      if (!cond_value) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "failed to generate condition expression")};
      }

      auto const cond = common.builder.CreateICmp(
        llvm::ICmpInst::ICMP_NE,
        cond_value,
        llvm::ConstantInt::get(
          common.kind_to_type(BuiltinType{BuiltinTypeKind::bool_}).type,
          0));

      common.builder.CreateCondBr(cond, body_bb, loop_end_bb);
    }
    else {
      // If condition is absent, unconditionally true.
      common.builder.CreateCondBr(
        llvm::ConstantInt::get(common.builder.getInt1Ty(), true),
        body_bb,
        loop_end_bb);
    }

    func->getBasicBlockList().push_back(body_bb);
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

    func->getBasicBlockList().push_back(loop_bb);
    common.builder.SetInsertPoint(loop_bb);

    if (node.loop_stmt)
      (*this)(*node.loop_stmt);

    common.builder.CreateBr(cond_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    common.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(ast::Break) const
  {
    // Whether in a loop.
    if (break_bb)
      common.builder.CreateBr(break_bb);
  }

  void operator()(ast::Continue) const
  {
    // Whether in a loop.
    if (continue_bb)
      common.builder.CreateBr(continue_bb);
  }

private:
  CodegenContext& common;

  SymbolTable& scope;

  // Used to combine returns into one.
  llvm::AllocaInst* retvar;
  llvm::BasicBlock* end_bb;

  // If not in loop, nullptr.
  llvm::BasicBlock* break_bb; // Basic block transitioned by break statement.
  llvm::BasicBlock*
    continue_bb; // Basic block transitioned by continue statement.
};

void codegen_statement(const ast::Stmt&   statement,
                       const SymbolTable& scope,
                       CodegenContext&    common,
                       llvm::AllocaInst*  retvar,
                       llvm::BasicBlock*  end_bb,
                       llvm::BasicBlock*  break_bb,
                       llvm::BasicBlock*  continue_bb)
{
  SymbolTable new_scope = scope;

  // Compound statement
  if (statement.type() == typeid(ast::CompoundStmt)) {
    auto& statements = boost::get<ast::CompoundStmt>(statement);

    for (const auto& r : statements) {
      boost::apply_visitor(
        StmtVisitor{common, new_scope, retvar, end_bb, break_bb, continue_bb},
        r);

      // If a terminator is present, subsequent code generation is
      // terminated.
      if (common.builder.GetInsertBlock()->getTerminator())
        break;
    }

    return;
  }

  // Other than compound statement
  boost::apply_visitor(
    StmtVisitor{common, new_scope, retvar, end_bb, break_bb, continue_bb},
    statement);
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CodegenContext&                    common,
                  llvm::legacy::FunctionPassManager& fpm)
    : common{common}
    , fpm{fpm}
  {
  }

  llvm::Function* operator()(ast::Nil) const
  {
    throw std::runtime_error{format_error_message(
      common.file.string(),
      "a function was executed that did not predict execution")};
  }

  llvm::Function* operator()(const ast::FunctionDecl& node) const
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
      const auto& param_type = node.params[i].type;
      param_types.at(i)      = common.kind_to_type(*param_type).type;
    }

    auto const func_type
      = llvm::FunctionType::get(common.kind_to_type(*node.return_type).type,
                                param_types,
                                is_vararg);

    llvm::Function* func;
    if (!node.linkage) {
      // External linkage.
      func = llvm::Function::Create(func_type,
                                    llvm::Function::ExternalLinkage,
                                    node.name,
                                    *common.module);
    }
    else if (node.linkage == Linkage::internal) {
      // Internal linkage.
      func = llvm::Function::Create(func_type,
                                    llvm::Function::InternalLinkage,
                                    node.name,
                                    *common.module);
    }

    // Set names for all arguments.
    for (std::size_t idx = 0; auto&& arg : func->args())
      arg.setName(node.params[idx++].name);

    return func;
  }

  llvm::Function* operator()(const ast::FunctionDef& node) const
  {
    auto func = common.module->getFunction(node.decl.name);

    if (!func)
      func = this->operator()(node.decl);

    if (!func) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("failed to create function %s", node.decl.name))};
    }

    SymbolTable argument_values;

    auto const entry_bb = llvm::BasicBlock::Create(*common.context, "", func);
    common.builder.SetInsertPoint(entry_bb);

    for (auto&& arg : func->args()) {
      const auto& param_node = node.decl.params[arg.getArgNo()];

      const auto arg_type = common.kind_to_type(*param_node.type);

      // Create an alloca for this variable.
      auto const inst
        = create_entry_block_alloca(func, arg.getName().str(), arg_type.type);

      // Store the initial value into the alloca.
      common.builder.CreateStore(&arg, inst);

      // Add arguments to variable symbol table.
      if (!param_node.qualifier) {
        // consttant variable.
        argument_values.regist(arg.getName().str(), {inst, false});
      }
      else if (*param_node.qualifier == VariableQual::mutable_) {
        // mutable variable.
        argument_values.regist(arg.getName().str(), {inst, true});
      }
    }

    const auto return_type = common.kind_to_type(*node.decl.return_type);

    // Used to combine returns into one.
    auto const end_bb = llvm::BasicBlock::Create(*common.context);
    auto const retvar
      = node.decl.return_type->getKind() == BuiltinTypeKind::void_
          ? nullptr
          : create_entry_block_alloca(func, "", return_type.type);

    codegen_statement(node.body,
                      argument_values,
                      common,
                      retvar,
                      end_bb,
                      nullptr,
                      nullptr);

    // If there is no return, returns undef.
    if (!common.builder.GetInsertBlock()->getTerminator()
        && node.decl.return_type->getKind() != BuiltinTypeKind::void_) {
      // Return 0 specially for main.
      if (node.decl.name == "main") {
        common.builder.CreateStore(
          llvm::ConstantInt::getSigned(func->getReturnType(), 0),
          retvar);
        common.builder.CreateBr(end_bb);
      }
      else {
        common.builder.CreateStore(llvm::UndefValue::get(func->getReturnType()),
                                   retvar);
        common.builder.CreateBr(end_bb);
      }
    }

    // Inserts a terminator if the function returning void does not have
    // one.
    if (node.decl.return_type->getKind() == BuiltinTypeKind::void_
        && !common.builder.GetInsertBlock()->getTerminator()) {
      common.builder.CreateBr(end_bb);
    }

    // Return.
    func->getBasicBlockList().push_back(end_bb);
    common.builder.SetInsertPoint(end_bb);

    if (retvar) {
      auto const retval
        = common.builder.CreateLoad(retvar->getAllocatedType(), retvar);
      common.builder.CreateRet(retval);
    }
    else {
      // Function that returns void.
      common.builder.CreateRet(nullptr);
    }

    std::string              em;
    llvm::raw_string_ostream os{em};
    if (llvm::verifyFunction(*func, &os)) {
      func->eraseFromParent();

      throw std::runtime_error{
        common.format_error(common.positions.position_of(node), os.str())};
    }

    fpm.run(*func);

    return func;
  }

private:
  CodegenContext& common;

  llvm::legacy::FunctionPassManager& fpm;
};

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

CodegenContext::CodegenContext(const std::filesystem::path& file,
                               const PositionCache&         positions)
  : context{std::make_unique<llvm::LLVMContext>()}
  , module{std::make_unique<llvm::Module>(file.filename().string(), *context)}
  , builder{*context}
  , file{file}
  , positions{positions}
{
}

[[nodiscard]] LLVMTypeWithSign CodegenContext::kind_to_type(const Type& type)
{
  return {type.getType(builder), type.isSigned()};
}

[[nodiscard]] llvm::Value* CodegenContext::i1_to_boolean(llvm::Value* value)
{
  const auto as = kind_to_type(BuiltinType{BuiltinTypeKind::bool_});

  return llvm::CastInst::CreateIntegerCast(value,
                                           as.type,
                                           as.is_signed,
                                           "",
                                           builder.GetInsertBlock());
}

[[nodiscard]] std::string
CodegenContext::format_error(const boost::iterator_range<InputIterator> pos,
                             const std::string_view                     message,
                             const bool with_code)
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

CodeGenerator::CodeGenerator(const std::string_view       program_name,
                             const ast::Program&          ast,
                             const PositionCache&         positions,
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
    // Initialize path manager.
    fpm.add(llvm::createInstructionCombiningPass());
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());
    fpm.add(llvm::createPromoteMemoryToRegisterPass());
    fpm.add(llvm::createInstructionCombiningPass());
    fpm.add(llvm::createReassociatePass());
  }

  fpm.doInitialization();

  // Set target triple and data layout to module.
  const auto target_triple = llvm::sys::getDefaultTargetTriple();

  std::string target_triple_error;
  auto const  target
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

void CodeGenerator::write_llvm_ir_to_file(
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

void CodeGenerator::write_object_code_to_file(const std::filesystem::path& out)
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
[[nodiscard]] int CodeGenerator::jit_compile()
{
  auto jit_expected = jit::JitCompiler::create();
  if (auto err = jit_expected.takeError()) {
    throw std::runtime_error{
      format_error_message(common.file.string(),
                           llvm::toString(std::move(err)),
                           true)};
  }

  auto jit = std::move(*jit_expected);
  if (auto err = jit->add_module(
        {std::move(common.module), std::move(common.context)})) {
    throw std::runtime_error{
      format_error_message(common.file.string(),
                           llvm::toString(std::move(err)))};
  }

  auto symbol_expected = jit->lookup("main");
  if (auto err = symbol_expected.takeError()) {
    throw std::runtime_error{
      format_error_message(common.file.string(),
                           "Symbol main could not be found")};
  }

  auto       symbol = *symbol_expected;
  auto const main_addr
    = reinterpret_cast<int (*)(/* TODO: command line arguments */)>(
      symbol.getAddress());

  // Run main.
  return main_addr();
}

void CodeGenerator::codegen()
{
  for (const auto& node : ast)
    boost::apply_visitor(TopLevelVisitor{common, fpm}, node);
}

} // namespace miko::codegen
