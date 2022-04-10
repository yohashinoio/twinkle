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

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

struct VariableInfo {
  llvm::AllocaInst* alloca;
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
  bool exists(const std::string& name) const
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

// Class that wraps llvm::Value.
// Made to handle signs, etc.
struct Value {
  Value(llvm::Value* value, const bool is_signed) noexcept
    : value{value}
    , is_signed{is_signed}
  {
  }

  explicit Value(llvm::Value* value) noexcept
    : value{value}
    , is_signed{false}
  {
  }

  Value() noexcept = default;

  [[nodiscard]] llvm::Value* getValue() const noexcept
  {
    return value;
  }

  [[nodiscard]] bool isSigned() const noexcept
  {
    return is_signed;
  }

  [[nodiscard]] bool isInteger() const
  {
    return value->getType()->isIntegerTy();
  }

  [[nodiscard]] explicit operator bool() const noexcept
  {
    return value;
  }

private:
  llvm::Value* value;
  bool         is_signed;
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

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CodegenContext& common, SymbolTable& scope)
    : common{common}
    , scope{scope}
  {
  }

  [[nodiscard]] Value operator()(ast::Nil) const
  {
    unreachable();
  }

  // 32bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint32_t node) const
  {
    return {llvm::ConstantInt::get(common.builder.getInt32Ty(), node), false};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(common.builder.getInt32Ty(), node),
            true};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(common.builder.getInt64Ty(), node), false};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(common.builder.getInt64Ty(), node),
            true};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {common.int1_to_bool(
              llvm::ConstantInt::get(common.builder.getInt1Ty(), node)),
            false};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return Value{common.builder.CreateGlobalStringPtr(node.str)};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Char literal is u8.
    return {llvm::ConstantInt::get(common.builder.getInt8Ty(), node.ch), false};
  }

  [[nodiscard]] Value operator()(const ast::UnaryOp& node) const
  {
    auto const rhs = boost::apply_visitor(*this, node.rhs);

    if (!rhs.getValue()) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side")};
    }

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to (0 - x).
      return Value{common.builder.CreateSub(
        llvm::ConstantInt::get(common.builder.getInt32Ty(), 0),
        rhs.getValue())};
    }

    throw std::runtime_error{
      common.format_error(common.positions.position_of(node),
                          format("unknown operator '%s' detected", node.op))};
  }

  [[nodiscard]] Value operator()(const ast::BinOp& node) const
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
    if (const auto lhs_bitwidth
        = lhs.getValue()->getType()->getIntegerBitWidth(),
        rhs_bitwidth = rhs.getValue()->getType()->getIntegerBitWidth();
        lhs_bitwidth != rhs_bitwidth) {
      const auto max_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

      const auto as = common.builder.getIntNTy(max_bitwidth);

      const auto as_is_signed
        = lhs_bitwidth == max_bitwidth ? lhs.isSigned() : rhs.isSigned();

      if (lhs_bitwidth == max_bitwidth) {
        rhs = {common.builder.CreateIntCast(rhs.getValue(), as, as_is_signed),
               as_is_signed};
      }
      else {
        lhs = {common.builder.CreateIntCast(lhs.getValue(), as, as_is_signed),
               as_is_signed};
      }
    }

    if (lhs.getValue()->getType() != rhs.getValue()->getType()) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        "both operands to a binary operator are not of the same type",
        false)};
    }

    // If either one of them is signed, the result is also signed.
    const auto result_is_signed
      = lhs.isSigned() || rhs.isSigned() ? true : false;

    // Addition.
    if (node.op == "+") {
      return {common.builder.CreateAdd(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }

    // Subtraction.
    if (node.op == "-") {
      return {common.builder.CreateSub(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }

    // Multiplication.
    if (node.op == "*") {
      return {common.builder.CreateMul(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }

    // Division.
    if (node.op == "/") {
      if (result_is_signed) {
        return {common.builder.CreateSDiv(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
      else {
        return {common.builder.CreateUDiv(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
    }

    // Modulo.
    if (node.op == "%") {
      if (result_is_signed) {
        return {common.builder.CreateSRem(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
      else {
        return {common.builder.CreateURem(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
    }

    // Equal.
    if (node.op == "==") {
      return Value{
        common.int1_to_bool(common.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                                      lhs.getValue(),
                                                      rhs.getValue()))};
    }

    // Not equal.
    if (node.op == "!=") {
      return Value{
        common.int1_to_bool(common.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                                      lhs.getValue(),
                                                      rhs.getValue()))};
    }

    // Less than.
    if (node.op == "<") {
      return Value{common.int1_to_bool(common.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SLT : llvm::ICmpInst::ICMP_ULT,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Greater than.
    if (node.op == ">") {
      return Value{common.int1_to_bool(common.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SGT : llvm::ICmpInst::ICMP_UGT,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Less or equal.
    if (node.op == "<=") {
      return Value{common.int1_to_bool(common.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SLE : llvm::ICmpInst::ICMP_ULE,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Greater or equal.
    if (node.op == ">=") {
      return Value{common.int1_to_bool(common.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SGE : llvm::ICmpInst::ICMP_UGE,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Unsupported binary operators detected.
    throw std::runtime_error{
      common.format_error(common.positions.position_of(node),
                          format("unknown operator '%s' detected", node.op),
                          false)};
  }

  [[nodiscard]] Value operator()(const ast::VariableRef& node) const
  {
    auto var_info = scope[node.name];

    if (!var_info) {
      throw std::runtime_error{common.format_error(
        common.positions.position_of(node),
        format("unknown variable '%s' referenced", node.name))};
    }

    return {common.builder.CreateLoad(var_info->alloca->getAllocatedType(),
                                      var_info->alloca),
            var_info->is_signed};
  }

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const
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
      args_value.push_back(
        boost::apply_visitor(*this, node.args[i]).getValue());

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

    return Value{common.builder.CreateCall(callee_func, args_value)};
  }

  [[nodiscard]] Value operator()(const ast::Conversion& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate left-hand side")};
    }

    return {common.builder.CreateIntCast(lhs.getValue(),
                                         node.as->getType(common.builder),
                                         node.as->isSigned()),
            node.as->isSigned()};
  }

  [[nodiscard]] Value operator()(const ast::AddressOf& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side")};
    }

    return Value{llvm::getPointerOperand(lhs.getValue())};
  }

  [[nodiscard]] Value operator()(const ast::Indirection& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "failed to generate right-hand side")};
    }

    auto const lhs_type = lhs.getValue()->getType();

    if (!lhs_type->isPointerTy()) {
      throw std::runtime_error{
        common.format_error(common.positions.position_of(node),
                            "unary '*' requires pointer operand")};
    }

    return {common.builder.CreateLoad(lhs_type->getPointerElementType(),
                                      lhs.getValue()),
            lhs.isSigned()};
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
          != retval.getValue()->getType()) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "incompatible type for result type")};
      }

      if (!retval) {
        throw std::runtime_error{
          common.format_error(common.positions.position_of(node),
                              "failed to generate return value")};
      }

      common.builder.CreateStore(retval.getValue(), retvar);
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
      = [&](llvm::AllocaInst* const alloca, const bool is_signed) {
          if (!node.qualifier) {
            // Consttant variable.
            scope.regist(node.name, {alloca, false, is_signed});
          }
          else if (*node.qualifier == VariableQual::mutable_) {
            // Mutable variable.
            scope.regist(node.name, {alloca, true, is_signed});
          }
        };

    if (node.type) {
      const auto& type_info = **node.type;

      auto const alloca
        = create_entry_block_alloca(func,
                                    node.name,
                                    type_info.getType(common.builder));

      if (node.initializer) {
        auto const init_value
          = boost::apply_visitor(ExprVisitor{common, scope}, *node.initializer);

        if (!init_value) {
          throw std::runtime_error{common.format_error(
            common.positions.position_of(node),
            format("failed to generate initializer for '%s'", node.name))};
        }

        if (type_info.getType(common.builder)
            != init_value.getValue()->getType()) {
          throw std::runtime_error{common.format_error(
            common.positions.position_of(node),
            "Initializer type and variable type are different")};
        }

        common.builder.CreateStore(init_value.getValue(), alloca);
      }

      regist(alloca, type_info.isSigned());
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

      auto const alloca
        = create_entry_block_alloca(func,
                                    node.name,
                                    init_value.getValue()->getType());

      common.builder.CreateStore(init_value.getValue(), alloca);

      regist(alloca, init_value.isSigned());
    }
  }

  void operator()(const ast::Assignment& node) const
  {
    if (node.op == "=" || node.op == "+=" || node.op == "-=" || node.op == "*="
        || node.op == "/=" || node.op == "%=") {
      Value lhs;

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

        lhs = {var_info->alloca, var_info->is_signed};
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

      if (!lhs.getValue()->getType()->isPointerTy()) {
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

      if (lhs.getValue()->getType()->getPointerElementType()
          != rhs.getValue()->getType()) {
        throw std::runtime_error{common.format_error(
          common.positions.position_of(node),
          "both operands to a binary operator are not of the same type")};
      }

      auto const lhs_value = common.builder.CreateLoad(
        lhs.getValue()->getType()->getPointerElementType(),
        lhs.getValue());

      // Direct assignment.
      if (node.op == "=")
        common.builder.CreateStore(rhs.getValue(), lhs.getValue());

      // Addition assignment.
      if (node.op == "+=") {
        common.builder.CreateStore(
          common.builder.CreateAdd(lhs_value, rhs.getValue()),
          lhs.getValue());
      }

      // Subtraction assignment.
      if (node.op == "-=") {
        common.builder.CreateStore(
          common.builder.CreateSub(lhs_value, rhs.getValue()),
          lhs.getValue());
      }

      // Multiplication assignment.
      if (node.op == "*=") {
        common.builder.CreateStore(
          common.builder.CreateMul(lhs_value, rhs.getValue()),
          lhs.getValue());
      }

      const auto result_is_signed
        = rhs.isSigned() || lhs.isSigned() ? true : false;

      // Division assignment.
      if (node.op == "/=") {
        auto const assign_value
          = result_is_signed
              ? common.builder.CreateSDiv(lhs_value, rhs.getValue())
              : common.builder.CreateUDiv(lhs_value, rhs.getValue());

        common.builder.CreateStore(assign_value, lhs.getValue());
      }

      // Modulo assignment.
      if (node.op == "%=") {
        auto const assign_value
          = result_is_signed
              ? common.builder.CreateSRem(lhs_value, rhs.getValue())
              : common.builder.CreateURem(lhs_value, rhs.getValue());

        common.builder.CreateStore(assign_value, lhs.getValue());
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
      cond_value.getValue(),
      llvm::ConstantInt::get(
        BuiltinType{BuiltinTypeKind::bool_}.getType(common.builder),
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
      cond_value.getValue(),
      llvm::ConstantInt::get(
        BuiltinType{BuiltinTypeKind::bool_}.getType(common.builder),
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
        cond_value.getValue(),
        llvm::ConstantInt::get(
          BuiltinType{BuiltinTypeKind::bool_}.getType(common.builder),
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
                  llvm::legacy::FunctionPassManager& fp_manager)
    : common{common}
    , fp_manager{fp_manager}
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
      param_types.at(i)      = param_type->getType(common.builder);
    }

    auto const func_type
      = llvm::FunctionType::get(node.return_type->getType(common.builder),
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

      // Create an alloca for this variable.
      auto const alloca
        = create_entry_block_alloca(func,
                                    arg.getName().str(),
                                    param_node.type->getType(common.builder));

      // Store the initial value into the alloca.
      common.builder.CreateStore(&arg, alloca);

      // Add arguments to variable symbol table.
      if (!param_node.qualifier) {
        // consttant variable.
        argument_values.regist(arg.getName().str(), {alloca, false});
      }
      else if (*param_node.qualifier == VariableQual::mutable_) {
        // mutable variable.
        argument_values.regist(arg.getName().str(), {alloca, true});
      }
    }

    // Used to combine returns into one.
    auto const end_bb = llvm::BasicBlock::Create(*common.context);
    auto const retvar
      = node.decl.return_type->getKind() == BuiltinTypeKind::void_
          ? nullptr
          : create_entry_block_alloca(
            func,
            "",
            node.decl.return_type->getType(common.builder));

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

    fp_manager.run(*func);

    return func;
  }

private:
  CodegenContext& common;

  llvm::legacy::FunctionPassManager& fp_manager;
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

[[nodiscard]] llvm::Value* CodegenContext::int1_to_bool(llvm::Value* value)
{
  const BuiltinType as{BuiltinTypeKind::bool_};

  return llvm::CastInst::CreateIntegerCast(value,
                                           as.getType(builder),
                                           as.isSigned(),
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
                             const bool                   opt,
                             const llvm::Reloc::Model     relocation_model)
  : program_name{program_name}
  , common{file, positions}
  , fp_manager{common.module.get()}
  , ast{ast}
{
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  if (opt) {
    // Initialize pass manager.
    fp_manager.add(llvm::createInstructionCombiningPass());
    fp_manager.add(llvm::createReassociatePass());
    fp_manager.add(llvm::createGVNPass());
    fp_manager.add(llvm::createCFGSimplificationPass());
    fp_manager.add(llvm::createPromoteMemoryToRegisterPass()); // mem2reg
    fp_manager.add(llvm::createInstructionCombiningPass());
    fp_manager.add(llvm::createReassociatePass());
  }

  fp_manager.doInitialization();

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

  target_machine = target->createTargetMachine(
    target_triple,
    "generic",
    "",
    target_options,
    llvm::Optional<llvm::Reloc::Model>(relocation_model));

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

  llvm::legacy::PassManager pmanager;

  if (target_machine->addPassesToEmitFile(pmanager,
                                          os,
                                          nullptr,
                                          llvm::CGFT_ObjectFile)) {
    throw std::runtime_error{
      format_error_message(program_name,
                           "targetMachine can't emit a file of this types",
                           true)};
  }

  pmanager.run(*common.module);
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
    boost::apply_visitor(TopLevelVisitor{common, fp_manager}, node);
}

} // namespace maple::codegen
