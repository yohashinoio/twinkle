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

[[nodiscard]] static Value createPointerToArray(CGContext&   ctx,
                                                const Value& array)
{
  return {llvm::getPointerOperand(array.getValue()),
          std::make_shared<PointerType>(array.getType()),
          array.isMutable()};
}

[[nodiscard]] static Value
createArraySubscript(CGContext& ctx, const Value& array, const Value& index)
{
  const auto p_to_array = createPointerToArray(ctx, array);

  // Calculate the address of the index-th element.
  auto const gep = ctx.builder.CreateInBoundsGEP(
    p_to_array.getLLVMType()->getPointerElementType(),
    p_to_array.getValue(),
    {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0), index.getValue()});

  return {gep,
          p_to_array.getType()->getPointeeType()->getArrayElementType(),
          p_to_array.isMutable()};
}

[[nodiscard]] static Value
createPointerSubscript(CGContext& ctx, const Value& ptr, const Value& index)
{
  // Calculate the address of the index-th element.
  auto const gep
    = ctx.builder.CreateInBoundsGEP(ptr.getLLVMType()->getPointerElementType(),
                                    ptr.getValue(),
                                    index.getValue());

  return {gep, ptr.getType()->getPointeeType(), ptr.isMutable()};
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
                      "the type incompatible with the subscript operator")};
  }

  const auto index = createExpr(ctx, scope, stmt_ctx, node.subscript);

  if (!index.isInteger()) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      "subscripts need to be evaluated to numbers")};
  }

  return is_array ? createArraySubscript(ctx, lhs, index)
                  : createPointerSubscript(ctx, lhs, index);
}

// This function changes the arguments.
// Compare both operands and cast to the operand with the larger bit width.
[[nodiscard]] static std::pair<Value, Value>
integerLargerBitsCast(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (const auto lhs_bitwidth = lhs.getLLVMType()->getIntegerBitWidth(),
      rhs_bitwidth            = rhs.getLLVMType()->getIntegerBitWidth();
      lhs_bitwidth != rhs_bitwidth) {
    const auto larger_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

    const auto target = ctx.builder.getIntNTy(larger_bitwidth);

    const auto is_target_lhs = lhs_bitwidth == larger_bitwidth;

    const auto target_type = resultTypeOf(lhs.getType(), rhs.getType());

    if (is_target_lhs) {
      return std::make_pair(
        lhs,
        Value{ctx.builder.CreateIntCast(rhs.getValue(),
                                        target,
                                        target_type->isSigned()),
              target_type});
    }
    else {
      return std::make_pair(
        Value{ctx.builder.CreateIntCast(lhs.getValue(),
                                        target,
                                        target_type->isSigned()),
              target_type},
        rhs);
    }
  }

  return std::make_pair(lhs, rhs);
}

// Calculate the offset of an element of a structure.
// Returns std::nullopt if there is no matching element.
[[nodiscard]] static std::optional<std::size_t>
offsetByName(const std::vector<ast::StructElement>& elements,
             const std::string&                     element_name)
{
  for (std::size_t offset = 0; const auto& element : elements) {
    if (element.name.utf8() == element_name)
      return offset;
    ++offset;
  }

  return std::nullopt;
}

[[nodiscard]] static std::vector<llvm::Value*>
value2LLVMValue(const std::vector<Value>& v)
{
  std::vector<llvm::Value*> ret;

  for (const auto& value : v)
    ret.push_back(value.getValue());

  return ret;
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
            std::make_shared<BuiltinType>(BuiltinTypeKind::u32)};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt32Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::i32)};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::u64)};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt64Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::i64)};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {llvm::ConstantInt::get(
              BuiltinType{BuiltinTypeKind::bool_}.getLLVMType(ctx),
              node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return {
      ctx.builder.CreateGlobalStringPtr(unicode::utf32toUtf8(node.str), ".str"),
      std::make_shared<PointerType>(
        std::make_shared<BuiltinType>(BuiltinTypeKind::i8))};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Unicode code point.
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node.ch),
            std::make_shared<BuiltinType>(BuiltinTypeKind::char_)};
  }

  [[nodiscard]] Value operator()(const ast::Identifier& node) const
  {
    // TODO: support function identifier
    const auto& variable = findVariable(ctx, node, scope);

    return {ctx.builder.CreateLoad(variable.getAllocaInst()->getAllocatedType(),
                                   variable.getAllocaInst()),
            variable.getType(),
            variable.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::MemberAccess& node) const
  {
    const auto lhs = createExpr(ctx, scope, stmt_ctx, node.lhs);

    if (!lhs.getLLVMType()->isStructTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "element selection cannot be used for non-structures")};
    }

    const auto struct_info
      = ctx.struct_table[lhs.getLLVMType()->getStructName().str()];

    if (!struct_info->first) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "element selection cannot be performed on undefined structures")};
    }

    const auto offset
      = offsetByName(struct_info->first.value(), node.selected_element.utf8());

    if (!offset) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        fmt::format("undefined element '{}' selected",
                                    node.selected_element.utf8()))};
    }

    auto const lhs_address = llvm::getPointerOperand(lhs.getValue());

    auto const gep = ctx.builder.CreateInBoundsGEP(
      lhs.getLLVMType(),
      lhs_address,
      {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
       llvm::ConstantInt::get(ctx.builder.getInt32Ty(), *offset)});

    return {
      ctx.builder.CreateLoad(lhs.getLLVMType()->getStructElementType(*offset),
                             gep),
      struct_info->first->at(*offset).type,
      lhs.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::Subscript& node) const
  {
    const auto value = createNoLoadSubscript(ctx, scope, stmt_ctx, node);

    return {ctx.builder.CreateLoad(value.getLLVMType()->getPointerElementType(),
                                   value.getValue()),
            value.getType(),
            value.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::BinOp& node) const
  {
    const auto uncasted_lhs = boost::apply_visitor(*this, node.lhs);
    const auto uncasted_rhs = boost::apply_visitor(*this, node.rhs);

    if (!uncasted_lhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate left-hand side")};
    }

    if (!uncasted_rhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate right-hand side")};
    }

    const auto [lhs, rhs]
      = integerLargerBitsCast(ctx, uncasted_lhs, uncasted_rhs);

    if (!strictEquals(lhs.getLLVMType(), rhs.getLLVMType())) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "both operands to a binary operator are not of the same type")};
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
        fmt::format("unknown operator '{}' detected", node.operatorStr()))};
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
                        "left-hand side of function call is not callable")};
    }

    const auto args = createArgValues(node.args, pos);

    const auto callee_name = boost::get<ast::Identifier>(node.callee).utf8();

    auto const callee_func = matchFunction(callee_name, args);

    if (!callee_func) {
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("unknown function '{}' referenced", callee_name))};
    }

    if (!callee_func->isVarArg()
        && callee_func->arg_size() != node.args.size()) {
      throw CodegenError{ctx.formatError(pos, "incorrect arguments passed")};
    }

    verifyArguments(args, callee_func, pos);

    const auto return_type = ctx.return_type_table[callee_func];

    assert(return_type);

    return {ctx.builder.CreateCall(callee_func, value2LLVMValue(args)),
            *return_type};
  }

  [[nodiscard]] Value operator()(const ast::Conversion& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate left-hand side")};
    }

    // TODO: Support for non-integers and non-pointers.
    if (node.as->getLLVMType(ctx)->isIntegerTy()) {
      return {ctx.builder.CreateIntCast(lhs.getValue(),
                                        node.as->getLLVMType(ctx),
                                        node.as->isSigned()),
              node.as};
    }
    else if (node.as->isPointerTy()) {
      // FIXME: I would like to prohibit this in the regular cast because it is
      // a dangerous cast.
      return {ctx.builder.CreatePointerCast(lhs.getValue(),
                                            node.as->getLLVMType(ctx)),
              node.as};
    }
    else {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "non-convertible type")};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::Pipeline& node) const
  {
    if (node.rhs.type() != typeid(ast::FunctionCall)) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "the right side of the pipeline requires a function call")};
    }

    // Copy.
    auto call = boost::get<ast::FunctionCall>(node.rhs);

    call.args.push_front(node.lhs);

    return this->operator()(call);
  }

private:
  [[nodiscard]] Value createAddressOf(const Value& value) const
  {
    return {llvm::getPointerOperand(value.getValue()),
            std::make_shared<PointerType>(value.getType())};
  }

  [[nodiscard]] Value
  createDereference(const boost::iterator_range<InputIterator>& pos,
                    const Value&                                rhs) const
  {
    if (!rhs.isPointer() || !rhs.getType()->isPointerTy()) {
      throw CodegenError{
        ctx.formatError(pos, "unary '*' requires pointer operand")};
    }

    return {ctx.builder.CreateLoad(rhs.getLLVMType()->getPointerElementType(),
                                   rhs.getValue()),
            rhs.getType()->getPointeeType(),
            rhs.isMutable()};
  }

  void verifyArguments(const std::vector<Value>&                   args,
                       llvm::Function* const                       callee,
                       const boost::iterator_range<InputIterator>& pos) const
  {
    for (std::size_t idx = 0; auto&& arg : callee->args()) {
      if (!strictEquals(args[idx++].getValue()->getType(), arg.getType())) {
        throw CodegenError{ctx.formatError(
          pos,
          fmt::format("incompatible type for argument {} of '{}'",
                      idx,
                      callee->getName()))};
      }
    }
  }

  [[nodiscard]] std::vector<Value>
  createArgValues(const std::deque<ast::Expr>&                arg_exprs,
                  const boost::iterator_range<InputIterator>& pos) const
  {
    std::vector<Value> args;

    for (std::size_t i = 0, size = arg_exprs.size(); i != size; ++i) {
      args.emplace_back(boost::apply_visitor(*this, arg_exprs[i]));

      if (!args.back()) {
        throw CodegenError{
          ctx.formatError(pos, "argument set failed in call to the function")};
      }
    }

    return args;
  }

  [[nodiscard]] llvm::Function*
  matchVarArgFunction(const std::string_view mangled_name) const
  {
    for (auto& func : ctx.module->getFunctionList()) {
      const auto func_name = func.getName();

      if (func_name.endswith("v")) {
        // _Z1fv to _Z1f
        const auto tmp = func_name.substr(0, func_name.size() - 2);
        if (mangled_name.starts_with(tmp))
          return &func;
      }
    }

    return nullptr;
  }

  [[nodiscard]] llvm::Function*
  matchFunction(const std::string_view    unmangled_name,
                const std::vector<Value>& args) const
  {
    {
      // First look for unmangled functions.
      auto const func = ctx.module->getFunction(unmangled_name);
      if (func)
        return func;
    }

    const auto mangled_name = ctx.mangler(unmangled_name, args);
    auto const func         = ctx.module->getFunction(mangled_name);

    if (!func) {
      // Mismatch or variadic arguments.
      auto const vararg_func = matchVarArgFunction(mangled_name);

      if (vararg_func)
        return vararg_func;
      else
        return nullptr;
    }

    return func;
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
