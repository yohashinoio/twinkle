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

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CGContext& ctx, SymbolTable& scope) noexcept;

  [[nodiscard]] Value operator()(ast::Nil) const
  {
    unreachable();
  }

  // 32bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint32_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node), false};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt32Ty(), node), true};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), node), false};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt64Ty(), node), true};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {
      ctx.int1ToBool(llvm::ConstantInt::get(ctx.builder.getInt1Ty(), node)),
      false};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return Value{ctx.builder.CreateGlobalStringPtr(node.str, ".str")};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Unicode code point.
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node.ch), false};
  }

  [[nodiscard]] Value operator()(const ast::Identifier& node) const;

  [[nodiscard]] Value operator()(const ast::BinOp& node) const;

  [[nodiscard]] Value operator()(const ast::UnaryOp& node) const;

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const;

  [[nodiscard]] Value operator()(const ast::Conversion& node) const;

private:
  [[nodiscard]] Value genAddressOf(const Value& rhs) const;

  [[nodiscard]] Value
  genIndirection(const boost::iterator_range<InputIterator>& pos,
                 const Value&                                rhs) const;

  CGContext& ctx;

  SymbolTable& scope;
};

ExprVisitor::ExprVisitor(CGContext& ctx, SymbolTable& scope) noexcept
  : ctx{ctx}
  , scope{scope}
{
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::Identifier& node) const
{
  // TODO: support function identifier
  const auto ident
    = utf32toUtf8cg(ctx, ctx.positions.position_of(node), node.name);

  const auto variable = scope[ident];

  if (!variable) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown variable '%s' referenced", ident))};
  }

  return {ctx.builder.CreateLoad(variable->getAllocaInst()->getAllocatedType(),
                                 variable->getAllocaInst()),
          variable->isSigned()};
}

// This function changes the value of the argument.
static void IntegerImplicitConversion(CGContext& ctx, Value& lhs, Value& rhs)
{
  if (const auto lhs_bitwidth = lhs.getType()->getIntegerBitWidth(),
      rhs_bitwidth            = rhs.getType()->getIntegerBitWidth();
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

  IntegerImplicitConversion(ctx, lhs, rhs);

  if (!equals(lhs.getType(), rhs.getType())) {
    throw std::runtime_error{ctx.formatError(
      ctx.positions.position_of(node),
      "both operands to a binary operator are not of the same type",
      false)};
  }

  if (node.isAddition())
    return genAddition(ctx, lhs, rhs);

  if (node.isSubtraction())
    return genSubtraction(ctx, lhs, rhs);

  if (node.isMultiplication())
    return genMultiplication(ctx, lhs, rhs);

  if (node.isDivision())
    return genDivision(ctx, lhs, rhs);

  if (node.isModulo())
    return genModulo(ctx, lhs, rhs);

  if (node.isEqual())
    return genEqual(ctx, lhs, rhs);

  if (node.isNotEqual())
    return genNotEqual(ctx, lhs, rhs);

  if (node.isLessThan())
    return genLessThan(ctx, lhs, rhs);

  if (node.isGreaterThan())
    return genGreaterThan(ctx, lhs, rhs);

  if (node.isLessOrEqual())
    return genLessOrEqual(ctx, lhs, rhs);

  if (node.isGreaterOrEqual())
    return genGreaterOrEqual(ctx, lhs, rhs);

  throw std::runtime_error{
    ctx.formatError(ctx.positions.position_of(node),
                    format("unknown operator '%s' detected", node.op),
                    false)};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::UnaryOp& node) const
{
  auto const rhs = boost::apply_visitor(*this, node.rhs);

  if (!rhs.getValue()) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate right-hand side")};
  }

  if (node.isUnaryPlus())
    return rhs;

  if (node.isUnaryMinus())
    return inverse(ctx, rhs);

  if (node.isIndirection())
    return genIndirection(ctx.positions.position_of(node), rhs);

  if (node.isAddressOf())
    return genAddressOf(rhs);

  throw std::runtime_error{
    ctx.formatError(ctx.positions.position_of(node),
                    format("unknown operator '%s' detected", node.op))};
}

[[nodiscard]] Value ExprVisitor::operator()(const ast::FunctionCall& node) const
{
  const auto callee
    = utf32toUtf8cg(ctx, ctx.positions.position_of(node), *node.callee);

  auto const callee_func = ctx.module->getFunction(callee);

  if (!callee_func) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown function '%s' referenced", callee))};
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
        format("argument set failed in call to the function '%s'", callee))};
    }
  }

  // Verify arguments
  for (std::size_t idx = 0; auto&& arg : callee_func->args()) {
    if (!equals(args_value[idx++]->getType(), arg.getType())) {
      throw std::runtime_error{ctx.formatError(
        ctx.positions.position_of(node),
        format("incompatible type for argument %d of '%s'", idx + 1, callee))};
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

[[nodiscard]] Value ExprVisitor::genAddressOf(const Value& rhs) const
{
  return Value{llvm::getPointerOperand(rhs.getValue())};
}

[[nodiscard]] Value
ExprVisitor::genIndirection(const boost::iterator_range<InputIterator>& pos,
                            const Value& rhs) const
{
  auto const rhs_type = rhs.getType();

  if (!rhs_type->isPointerTy()) {
    throw std::runtime_error{
      ctx.formatError(pos, "unary '*' requires pointer operand")};
  }

  return {
    ctx.builder.CreateLoad(rhs_type->getPointerElementType(), rhs.getValue()),
    rhs.isSigned()};
}

[[nodiscard]] Value
genExpr(CGContext& ctx, SymbolTable& scope, const ast::Expr& expr)
{
  return boost::apply_visitor(ExprVisitor{ctx, scope}, expr);
}

} // namespace maple::codegen
