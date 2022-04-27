/**
 * expr.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/expr.hpp>
#include <support/format.hpp>
#include <codegen/exception.hpp>

namespace maple::codegen
{

[[nodiscard]] Variable findVariable(CGContext&             ctx,
                                    const ast::Identifier& node,
                                    const SymbolTable&     scope)
{
  const auto ident = node.utf8();

  const auto variable = scope[ident];

  if (!variable) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown variable '%s' referenced", ident))};
  }

  return *variable;
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

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CGContext& ctx, SymbolTable& scope) noexcept
    : ctx{ctx}
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
    return {llvm::ConstantInt::get(
              BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.context),
              node),
            false};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return {ctx.builder.CreateGlobalStringPtr(unicode::utf32toUtf8(node.str),
                                              ".str")};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Unicode code point.
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node.ch), false};
  }

  [[nodiscard]] Value operator()(const ast::Identifier& node) const
  {
    // TODO: support function identifier
    const auto variable = findVariable(ctx, node, scope);

    return {ctx.builder.CreateLoad(variable.getAllocaInst()->getAllocatedType(),
                                   variable.getAllocaInst()),
            variable.isSigned(),
            variable.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::Subscript& node) const
  {
    auto lhs = this->operator()(node.ident);

    const auto is_array = lhs.getType()->isArrayTy();

    if (!is_array && !lhs.getType()->isPointerTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "the type incompatible with the subscript operator")};
    }

    if (is_array)
      lhs = createAddressOf(lhs);

    const auto nsubscript = boost::apply_visitor(*this, node.nsubscript);

    if (!nsubscript.isInteger()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "subscripts need to be evaluated to numbers")};
    }

    auto const gep = is_array ? ctx.builder.CreateInBoundsGEP(
                       lhs.getType()->getPointerElementType(),
                       lhs.getValue(),
                       {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
                        nsubscript.getValue()})
                              : ctx.builder.CreateInBoundsGEP(
                                lhs.getType()->getPointerElementType(),
                                lhs.getValue(),
                                nsubscript.getValue());

    auto const element_type
      = is_array ? lhs.getType()->getPointerElementType()->getArrayElementType()
                 : lhs.getType()->getPointerElementType();

    return {ctx.builder.CreateLoad(element_type, gep), lhs.isSigned()};
  }

  [[nodiscard]] Value operator()(const ast::BinOp& node) const
  {
    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (!lhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate left-hand side",
                                         false)};
    }

    if (!rhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate right-hand side",
                                         false)};
    }

    IntegerImplicitConversion(ctx, lhs, rhs);

    if (!strictEquals(lhs.getType(), rhs.getType())) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "both operands to a binary operator are not of the same type",
        false)};
    }

    switch (node.kind()) {
    case ast::BinOp::Kind::add:
      return createAdd(ctx, lhs, rhs);

    case ast::BinOp::Kind::sub:
      return createSub(ctx, lhs, rhs);

    case ast::BinOp::Kind::mul:
      return createMul(ctx, lhs, rhs);

    case ast::BinOp::Kind::div:
      return createDiv(ctx, lhs, rhs);

    case ast::BinOp::Kind::mod:
      return createMod(ctx, lhs, rhs);

    case ast::BinOp::Kind::eq:
      return createEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::neq:
      return createNotEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::lt:
      return createLessThan(ctx, lhs, rhs);

    case ast::BinOp::Kind::gt:
      return createGreaterThan(ctx, lhs, rhs);

    case ast::BinOp::Kind::le:
      return createLessOrEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::ge:
      return createGreaterOrEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        format("unknown operator '%s' detected", node.operatorStr()),
        false)};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::UnaryOp& node) const
  {
    auto const rhs = boost::apply_visitor(*this, node.rhs);

    if (!rhs.getValue()) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate right-hand side")};
    }

    switch (node.kind()) {
    case ast::UnaryOp::Kind::plus:
      return rhs;

    case ast::UnaryOp::Kind::minus:
      return createAddInverse(ctx, rhs);

    case ast::UnaryOp::Kind::not_:
      return createLogicalNot(ctx, rhs);

    case ast::UnaryOp::Kind::indirection:
      return createIndirection(ctx.positions.position_of(node), rhs);

    case ast::UnaryOp::Kind::address_of:
      return createAddressOf(rhs);

    case ast::UnaryOp::Kind::size_of:
      return createSizeOf(ctx, rhs);

    case ast::UnaryOp::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        format("unknown operator '%s' detected", node.operatorStr()))};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const
  {
    const auto callee = node.callee.utf8();

    auto const callee_func = ctx.module->getFunction(callee);

    if (!callee_func) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        format("unknown function '%s' referenced", callee))};
    }

    if (!callee_func->isVarArg()
        && callee_func->arg_size() != node.args.size()) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         format("incorrect arguments passed"))};
    }

    std::vector<llvm::Value*> args_value;
    for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
      args_value.push_back(
        boost::apply_visitor(*this, node.args[i]).getValue());

      if (!args_value.back()) {
        throw CodegenError{ctx.formatError(
          ctx.positions.position_of(node),
          format("argument set failed in call to the function '%s'", callee))};
      }
    }

    // Verify arguments
    for (std::size_t idx = 0; auto&& arg : callee_func->args()) {
      if (!strictEquals(args_value[idx++]->getType(), arg.getType())) {
        throw CodegenError{ctx.formatError(
          ctx.positions.position_of(node),
          format("incompatible type for argument %d of '%s'", idx, callee))};
      }
    }

    return {ctx.builder.CreateCall(callee_func, args_value)};
  }

  [[nodiscard]] Value operator()(const ast::Conversion& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
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
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        format("cannot be converted to '%s' type", node.as->getName()))};
    }

    unreachable();
  }

private:
  [[nodiscard]] Value createAddressOf(const Value& rhs) const
  {
    return {llvm::getPointerOperand(rhs.getValue())};
  }

  [[nodiscard]] Value
  createIndirection(const boost::iterator_range<InputIterator>& pos,
                    const Value&                                rhs) const
  {
    auto const rhs_type = rhs.getType();

    if (!rhs_type->isPointerTy()) {
      throw CodegenError{
        ctx.formatError(pos, "unary '*' requires pointer operand")};
    }

    return {
      ctx.builder.CreateLoad(rhs_type->getPointerElementType(), rhs.getValue()),
      rhs.isSigned(),
      rhs.isMutable()};
  }

  CGContext& ctx;

  SymbolTable& scope;
};

[[nodiscard]] Value
createExpr(CGContext& ctx, SymbolTable& scope, const ast::Expr& expr)
{
  return boost::apply_visitor(ExprVisitor{ctx, scope}, expr);
}

} // namespace maple::codegen
