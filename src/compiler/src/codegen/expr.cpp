/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/expr.hpp>
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
                      fmt::format("unknown variable '{}' referenced", ident))};
  }

  return *variable;
}

// This function changes the arguments.
// Compare both operands and cast to the operand with the larger bit width.
static void integerLargerBitsCast(CGContext& ctx, Value& lhs, Value& rhs)
{
  if (const auto lhs_bitwidth = lhs.getType()->getIntegerBitWidth(),
      rhs_bitwidth            = rhs.getType()->getIntegerBitWidth();
      lhs_bitwidth != rhs_bitwidth) {
    const auto larger_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

    const auto target = ctx.builder.getIntNTy(larger_bitwidth);

    const auto target_is_lhs = lhs_bitwidth == larger_bitwidth;

    const auto target_sign_kind
      = target_is_lhs ? lhs.getSignKind() : rhs.getSignKind();

    if (target_is_lhs) {
      rhs = {ctx.builder.CreateIntCast(rhs.getValue(),
                                       target,
                                       isSigned(target_sign_kind)),
             createStack(target_sign_kind)};
    }
    else {
      lhs = {ctx.builder.CreateIntCast(lhs.getValue(),
                                       target,
                                       isSigned(target_sign_kind)),
             createStack(target_sign_kind)};
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
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node),
            createStack(SignKind::unsigned_)};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt32Ty(), node),
            createStack(SignKind::signed_)};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), node),
            createStack(SignKind::unsigned_)};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt64Ty(), node),
            createStack(SignKind::signed_)};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {llvm::ConstantInt::get(
              BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.context),
              node),
            createStack(SignKind::unsigned_)};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return {
      ctx.builder.CreateGlobalStringPtr(unicode::utf32toUtf8(node.str), ".str"),
      createStack(SignKind::unsigned_)};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Unicode code point.
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node.ch),
            createStack(SignKind::unsigned_)};
  }

  [[nodiscard]] Value operator()(const ast::Identifier& node) const
  {
    // TODO: support function identifier
    const auto variable = findVariable(ctx, node, scope);

    return {ctx.builder.CreateLoad(variable.getAllocaInst()->getAllocatedType(),
                                   variable.getAllocaInst()),
            variable.getSignInfo(),
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

    auto sign_info_stack = lhs.getSignInfo();

    assert(2 <= sign_info_stack.size());
    sign_info_stack.pop();

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

    return {ctx.builder.CreateLoad(element_type, gep),
            std::move(sign_info_stack)};
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

    integerLargerBitsCast(ctx, lhs, rhs);

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
        fmt::format("unknown operator '{}' detected", node.operatorStr()),
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
        fmt::format("unknown operator '{}' detected", node.operatorStr()))};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const
  {
    const auto callee = node.callee.utf8();

    auto const callee_func = ctx.module->getFunction(callee);

    if (!callee_func) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        fmt::format("unknown function '{}' referenced", callee))};
    }

    if (!callee_func->isVarArg()
        && callee_func->arg_size() != node.args.size()) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "incorrect arguments passed")};
    }

    std::vector<llvm::Value*> args_value;
    for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
      args_value.push_back(
        boost::apply_visitor(*this, node.args[i]).getValue());

      if (!args_value.back()) {
        throw CodegenError{ctx.formatError(
          ctx.positions.position_of(node),
          fmt::format("argument set failed in call to the function '{}'",
                      callee))};
      }
    }

    // Verify arguments
    for (std::size_t idx = 0; auto&& arg : callee_func->args()) {
      if (!strictEquals(args_value[idx++]->getType(), arg.getType())) {
        throw CodegenError{ctx.formatError(
          ctx.positions.position_of(node),
          fmt::format("incompatible type for argument {} of '{}'",
                      idx,
                      callee))};
      }
    }

    // Get return type.
    const auto return_type = ctx.func_ret_types[callee];
    assert(return_type);

    return {ctx.builder.CreateCall(callee_func, args_value),
            return_type.value()->createSignKindStack()};
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
              createStack(node.as->getSignKind())};
    }
    else if (node.as->isPointer()) {
      // FIXME: I would like to prohibit this in the regular cast because it is
      // a dangerous cast.
      return {ctx.builder.CreatePointerCast(lhs.getValue(),
                                            node.as->getType(ctx.context)),
              createStack(node.as->getSignKind())};
    }
    else {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        fmt::format("cannot be converted to '{}' type", node.as->getName()))};
    }

    unreachable();
  }

private:
  [[nodiscard]] Value createAddressOf(const Value& rhs) const
  {
    auto tmp = rhs.getSignInfo();
    tmp.push(SignKind::unsigned_); // Pointer type.

    return {llvm::getPointerOperand(rhs.getValue()), std::move(tmp)};
  }

  [[nodiscard]] Value
  createIndirection(const boost::iterator_range<InputIterator>& pos,
                    const Value&                                rhs) const
  {
    auto tmp = rhs.getSignInfo();

    if (!rhs.isPointer() || tmp.size() < 2) {
      throw CodegenError{
        ctx.formatError(pos, "unary '*' requires pointer operand")};
    }

    tmp.pop();

    return {ctx.builder.CreateLoad(rhs.getType()->getPointerElementType(),
                                   rhs.getValue()),
            std::move(tmp),
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
