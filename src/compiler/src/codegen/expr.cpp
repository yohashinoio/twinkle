/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/codegen/expr.hpp>
#include <maple/codegen/exception.hpp>
#include <maple/codegen/stmt.hpp>

namespace maple::codegen
{

// Be careful about the lifetime of the return value references.
[[nodiscard]] Variable& findVariable(const CGContext&       ctx,
                                     const ast::Identifier& node,
                                     SymbolTable&           scope)
{
  const auto ident = node.utf8();

  if (const auto variable = scope[ident])
    return *variable;
  else {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      fmt::format("unknown variable '{}' referenced", ident))};
  }

  unreachable();
}

// Normally a subscript operation calls createLoad at the end, but this function
// does not.
[[nodiscard]] Value createNoLoadSubscript(CGContext&            ctx,
                                          SymbolTable&          scope,
                                          const StmtContext&    stmt_ctx,
                                          const ast::Subscript& node)
{
  auto lhs = createExpr(ctx, scope, stmt_ctx, node.lhs);

  const auto is_array = lhs.getType()->isArrayTy();

  if (!is_array && !lhs.getType()->isPointerTy()) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      "the type incompatible with the subscript operator",
                      false)};
  }

  if (is_array) {
    // Get the address of the first element of the array.
    auto tmp = lhs.getSignInfo();
    tmp.emplace(SignKind::unsigned_); // Pointer type.
    lhs = {llvm::getPointerOperand(lhs.getValue()),
           std::move(tmp),
           lhs.isMutable()};
  }

  const auto subscript = createExpr(ctx, scope, stmt_ctx, node.subscript);

  if (!subscript.isInteger()) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      "subscripts need to be evaluated to numbers",
                      false)};
  }

  // Calculate the address of the subscript-th element.
  auto const gep
    = is_array
        ? ctx.builder.CreateInBoundsGEP(
          lhs.getType()->getPointerElementType(),
          lhs.getValue(),
          {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
           subscript.getValue()})
        : ctx.builder.CreateInBoundsGEP(lhs.getType()->getPointerElementType(),
                                        lhs.getValue(),
                                        subscript.getValue());

  {
    // I did GEP and will pop.
    auto tmp = lhs.getSignInfo();

    if (is_array) {
      // Because the behavior of gep changes between pointers and arrays.
      tmp.pop();
    }

    return {gep, std::move(tmp), lhs.isMutable()};
  }
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

    const auto is_target_lhs = lhs_bitwidth == larger_bitwidth;

    const auto target_sign_kind
      = is_target_lhs ? lhs.getSignKind() : rhs.getSignKind();

    if (is_target_lhs) {
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

// Calculate the offset of an element of a structure.
static std::size_t offsetByName(const std::vector<ast::StructElement>& elements,
                                const std::string& element_name)
{
  for (std::size_t offset = 0; const auto& element : elements) {
    if (element.name.utf8() == element_name)
      return offset;
    ++offset;
  }

  unreachable();
}

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CGContext&         ctx,
              SymbolTable&       scope,
              const StmtContext& stmt_ctx) noexcept
    : ctx{ctx}
    , scope{scope}
    , stmt_ctx{stmt_ctx}
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
    return {
      llvm::ConstantInt::get(BuiltinType{BuiltinTypeKind::bool_}.getType(ctx),
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
    const auto& variable = findVariable(ctx, node, scope);

    return {ctx.builder.CreateLoad(variable.getAllocaInst()->getAllocatedType(),
                                   variable.getAllocaInst()),
            variable.getSignInfo(),
            variable.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::MemberAccess& node) const
  {
    const auto lhs = createExpr(ctx, scope, stmt_ctx, node.lhs);

    if (!lhs.getType()->isStructTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "element selection cannot be used for non-structures",
                        false)};
    }

    const auto struct_info
      = ctx.struct_table[lhs.getType()->getStructName().str()];

    if (!struct_info->first) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "element selection cannot be performed on undefined structures",
        false)};
    }

    const auto offset
      = offsetByName(struct_info->first.value(), node.selected_element.utf8());

    auto const lhs_address = llvm::getPointerOperand(lhs.getValue());

    auto const gep = ctx.builder.CreateInBoundsGEP(
      lhs.getType(),
      lhs_address,
      {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
       llvm::ConstantInt::get(ctx.builder.getInt32Ty(), offset)});

    return {
      ctx.builder.CreateLoad(lhs.getType()->getStructElementType(offset), gep),
      struct_info->first->at(offset).type->createSignKindStack(ctx),
      lhs.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::Subscript& node) const
  {
    const auto value = createNoLoadSubscript(ctx, scope, stmt_ctx, node);

    // Load, so pop.
    auto tmp = value.getSignInfo();
    tmp.pop();

    return {ctx.builder.CreateLoad(value.getType()->getPointerElementType(),
                                   value.getValue()),
            std::move(tmp),
            value.isMutable()};
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

    case ast::BinOp::Kind::logical_and:
      return createLogicalAnd(ctx, lhs, rhs);

    case ast::BinOp::Kind::logical_or:
      return createLogicalOr(ctx, lhs, rhs);

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

    case ast::UnaryOp::Kind::dereference:
      return createDereference(ctx.positions.position_of(node), rhs);

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
    const auto pos = ctx.positions.position_of(node);

    if (node.callee.type() != typeid(ast::Identifier)) {
      throw CodegenError{
        ctx.formatError(pos,
                        "left-hand side of function call is not callable",
                        false)};
    }

    const auto callee = boost::get<ast::Identifier>(node.callee).utf8();

    auto const callee_func = ctx.module->getFunction(callee);

    if (!callee_func) {
      throw CodegenError{
        ctx.formatError(pos,
                        fmt::format("unknown function '{}' referenced", callee),
                        false)};
    }

    if (!callee_func->isVarArg()
        && callee_func->arg_size() != node.args.size()) {
      throw CodegenError{
        ctx.formatError(pos, "incorrect arguments passed", false)};
    }

    const auto arg_values = createArgValues(node.args, callee, pos);

    verifyArguments(arg_values, callee_func, pos);

    const auto return_type = ctx.return_type_table[callee];
    assert(return_type);

    return {ctx.builder.CreateCall(callee_func, arg_values),
            return_type.value()->createSignKindStack(ctx)};
  }

  [[nodiscard]] Value operator()(const ast::Conversion& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate left-hand side",
                                         false)};
    }

    // TODO: Support for non-integers and non-pointers.
    if (node.as->getType(ctx)->isIntegerTy()) {
      return {ctx.builder.CreateIntCast(lhs.getValue(),
                                        node.as->getType(ctx),
                                        node.as->isSigned()),
              createStack(node.as->getSignKind())};
    }
    else if (node.as->isPointerTy()) {
      // FIXME: I would like to prohibit this in the regular cast because it is
      // a dangerous cast.
      return {
        ctx.builder.CreatePointerCast(lhs.getValue(), node.as->getType(ctx)),
        createStack(node.as->getSignKind())};
    }
    else {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "non-convertible type",
                                         false)};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::Pipeline& node) const
  {
    if (node.rhs.type() != typeid(ast::FunctionCall)) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "the right side of the pipeline requires a function call",
        false)};
    }

    // Copy.
    auto call = boost::get<ast::FunctionCall>(node.rhs);

    call.args.push_front(node.lhs);

    return this->operator()(call);
  }

private:
  [[nodiscard]] Value createAddressOf(const Value& value) const
  {
    auto tmp = value.getSignInfo();
    tmp.emplace(SignKind::unsigned_); // Pointer type.

    return {llvm::getPointerOperand(value.getValue()), std::move(tmp)};
  }

  [[nodiscard]] Value
  createDereference(const boost::iterator_range<InputIterator>& pos,
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

  void verifyArguments(const std::vector<llvm::Value*>&            args_value,
                       llvm::Function* const                       callee,
                       const boost::iterator_range<InputIterator>& pos) const
  {
    // Verify arguments
    for (std::size_t idx = 0; auto&& arg : callee->args()) {
      if (!strictEquals(args_value[idx++]->getType(), arg.getType())) {
        throw CodegenError{ctx.formatError(
          pos,
          fmt::format("incompatible type for argument {} of '{}'",
                      idx,
                      callee->getName()))};
      }
    }
  }

  [[nodiscard]] std::vector<llvm::Value*>
  createArgValues(const std::deque<ast::Expr>&                args,
                  const std::string_view                      callee,
                  const boost::iterator_range<InputIterator>& pos) const
  {
    std::vector<llvm::Value*> arg_values;

    for (std::size_t i = 0, size = args.size(); i != size; ++i) {
      arg_values.emplace_back(boost::apply_visitor(*this, args[i]).getValue());

      if (!arg_values.back()) {
        throw CodegenError{ctx.formatError(
          pos,
          fmt::format("argument set failed in call to the function '{}'",
                      callee))};
      }
    }

    return arg_values;
  }

  CGContext& ctx;

  SymbolTable& scope;

  const StmtContext& stmt_ctx;
};

[[nodiscard]] Value createExpr(CGContext&         ctx,
                               SymbolTable&       scope,
                               const StmtContext& stmt_ctx,
                               const ast::Expr&   expr)
{
  return boost::apply_visitor(ExprVisitor{ctx, scope, stmt_ctx}, expr);
}

} // namespace maple::codegen
