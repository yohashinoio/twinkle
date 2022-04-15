/**
 * expr.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/expr.hpp>
#include <utils/format.hpp>

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

ExprVisitor::ExprVisitor(CodeGenerator::Context& ctx,
                         SymbolTable&            scope) noexcept
  : ctx{ctx}
  , scope{scope}
{
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::InitList& node) const
{
  // TODO:
  unreachable();
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::UnaryOp& node) const
{
  auto const rhs = boost::apply_visitor(*this, node.rhs);

  if (!rhs.getValue()) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate right-hand side")};
  }

  if (node.op == "+")
    return rhs;
  if (node.op == "-") {
    // -x to (0 - x).
    return Value{
      ctx.builder.CreateSub(llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
                            rhs.getValue())};
  }

  throw std::runtime_error{
    ctx.formatError(ctx.positions.position_of(node),
                    format("unknown operator '%s' detected", node.op))};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::BinOp& node) const
{
  auto lhs = boost::apply_visitor(*this, node.lhs);
  auto rhs = boost::apply_visitor(*this, node.rhs);

  if (!lhs) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate left-hand side",
                      false)};
  }

  if (!rhs) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate right-hand side",
                      false)};
  }

  // Implicit conversions.
  if (const auto lhs_bitwidth = lhs.getValue()->getType()->getIntegerBitWidth(),
      rhs_bitwidth            = rhs.getValue()->getType()->getIntegerBitWidth();
      lhs_bitwidth != rhs_bitwidth) {
    const auto max_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

    const auto as = ctx.builder.getIntNTy(max_bitwidth);

    const auto as_is_signed
      = lhs_bitwidth == max_bitwidth ? lhs.isSigned() : rhs.isSigned();

    if (lhs_bitwidth == max_bitwidth) {
      rhs = {ctx.builder.CreateIntCast(rhs.getValue(), as, as_is_signed),
             as_is_signed};
    }
    else {
      lhs = {ctx.builder.CreateIntCast(lhs.getValue(), as, as_is_signed),
             as_is_signed};
    }
  }

  if (lhs.getValue()->getType() != rhs.getValue()->getType()) {
    throw std::runtime_error{ctx.formatError(
      ctx.positions.position_of(node),
      "both operands to a binary operator are not of the same type",
      false)};
  }

  // If either one of them is signed, the result is also signed.
  const auto result_is_signed = lhs.isSigned() || rhs.isSigned() ? true : false;

  // Addition.
  if (node.op == "+") {
    return {ctx.builder.CreateAdd(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }

  // Subtraction.
  if (node.op == "-") {
    return {ctx.builder.CreateSub(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }

  // Multiplication.
  if (node.op == "*") {
    return {ctx.builder.CreateMul(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }

  // Division.
  if (node.op == "/") {
    if (result_is_signed) {
      return {ctx.builder.CreateSDiv(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }
    else {
      return {ctx.builder.CreateUDiv(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }
  }

  // Modulo.
  if (node.op == "%") {
    if (result_is_signed) {
      return {ctx.builder.CreateSRem(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }
    else {
      return {ctx.builder.CreateURem(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }
  }

  // Equal.
  if (node.op == "==") {
    return Value{ctx.int1ToBool(ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                                       lhs.getValue(),
                                                       rhs.getValue()))};
  }

  // Not equal.
  if (node.op == "!=") {
    return Value{ctx.int1ToBool(ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                                       lhs.getValue(),
                                                       rhs.getValue()))};
  }

  // Less than.
  if (node.op == "<") {
    return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
      result_is_signed ? llvm::ICmpInst::ICMP_SLT : llvm::ICmpInst::ICMP_ULT,
      lhs.getValue(),
      rhs.getValue()))};
  }

  // Greater than.
  if (node.op == ">") {
    return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
      result_is_signed ? llvm::ICmpInst::ICMP_SGT : llvm::ICmpInst::ICMP_UGT,
      lhs.getValue(),
      rhs.getValue()))};
  }

  // Less or equal.
  if (node.op == "<=") {
    return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
      result_is_signed ? llvm::ICmpInst::ICMP_SLE : llvm::ICmpInst::ICMP_ULE,
      lhs.getValue(),
      rhs.getValue()))};
  }

  // Greater or equal.
  if (node.op == ">=") {
    return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
      result_is_signed ? llvm::ICmpInst::ICMP_SGE : llvm::ICmpInst::ICMP_UGE,
      lhs.getValue(),
      rhs.getValue()))};
  }

  // Unsupported binary operators detected.
  throw std::runtime_error{
    ctx.formatError(ctx.positions.position_of(node),
                    format("unknown operator '%s' detected", node.op),
                    false)};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::VariableRef& node) const
{
  auto variable = scope[node.name];

  if (!variable) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown variable '%s' referenced", node.name))};
  }

  return {ctx.builder.CreateLoad(variable->getAllocaInst()->getAllocatedType(),
                                 variable->getAllocaInst()),
          variable->isSigned()};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::FunctionCall& node) const
{
  auto const callee_func = ctx.module->getFunction(node.callee);

  if (!callee_func) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown function '%s' referenced", node.callee))};
  }

  if (!callee_func->isVarArg() && callee_func->arg_size() != node.args.size()) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("incorrect arguments passed"))};
  }

  std::vector<llvm::Value*> args_value;
  for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
    args_value.push_back(boost::apply_visitor(*this, node.args[i]).getValue());

    if (!args_value.back()) {
      throw std::runtime_error{ctx.formatError(
        ctx.positions.position_of(node),
        format("argument set failed in call to the function '%s'",
               node.callee))};
    }
  }

  // Verify arguments
  for (std::size_t idx = 0; auto&& arg : callee_func->args()) {
    if (args_value[idx++]->getType() != arg.getType()) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        format("incompatible type for argument %d of '%s'",
                               idx + 1,
                               node.callee))};
    }
  }

  return Value{ctx.builder.CreateCall(callee_func, args_value)};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::Conversion& node) const
{
  auto const lhs = boost::apply_visitor(*this, node.lhs);

  if (!lhs) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate left-hand side")};
  }

  // TODO: Support for non-integers and non-pointers.
  if (node.as->getType(ctx.context)->isIntegerTy()) {
    return {ctx.builder.CreateIntCast(lhs.getValue(),
                                      node.as->getType(ctx.context),
                                      node.as->isSigned()),
            node.as->isSigned()};
  }
  else if (node.as->isPointer()) {
    // FIXME: I would like to prohibit this in the regular cast because it is
    // a dangerous cast.
    return {ctx.builder.CreatePointerCast(lhs.getValue(),
                                          node.as->getType(ctx.context)),
            node.as->isSigned()};
  }
  else {
    throw std::runtime_error{ctx.formatError(
      ctx.positions.position_of(node),
      format("cannot be converted to '%s' type", node.as->getName()))};
  }

  unreachable();
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::AddressOf& node) const
{
  auto const lhs = boost::apply_visitor(*this, node.lhs);

  if (!lhs) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate right-hand side")};
  }

  return Value{llvm::getPointerOperand(lhs.getValue())};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::Indirection& node) const
{
  auto const lhs = boost::apply_visitor(*this, node.lhs);

  if (!lhs) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate right-hand side")};
  }

  auto const lhs_type = lhs.getValue()->getType();

  if (!lhs_type->isPointerTy()) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "unary '*' requires pointer operand")};
  }

  return {
    ctx.builder.CreateLoad(lhs_type->getPointerElementType(), lhs.getValue()),
    lhs.isSigned()};
}

} // namespace maple::codegen
