/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/codegen/common.hpp>

namespace twinkle::codegen
{

void TemplateArgumentsDefiner::insertToAliasTable(
  const PositionRange& pos) const
{
  TemplateArgumentTable template_argument_table;

  for (std::size_t idx = 0; const auto& param : params.type_names) {
    const auto param_name = param.utf8();

    if (template_argument_table.exists(param_name)) {
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("redefinition of template parameter '{}'", param_name))};
    }

    template_argument_table.insert(param_name,
                                   createType(ctx, args.types[idx], pos));
    ++idx;
  }

  ctx.template_argument_tables.emplace(std::move(template_argument_table));
}

[[nodiscard]] std::optional<std::pair<ClassTemplateTableValue, NamespaceStack>>
findClassTemplate(CGContext&                    ctx,
                  const std::string_view        name,
                  const ast::TemplateArguments& args)
{
  auto namespace_copy = ctx.ns_hierarchy;

  for (;;) {
    if (const auto value
        = ctx.class_template_table[TemplateTableKey{name,
                                                    args.types.size(),
                                                    namespace_copy}]) {
      return std::make_pair(*value, std::move(namespace_copy));
    }

    if (namespace_copy.empty())
      return std::nullopt;

    namespace_copy.pop();
  }

  unreachable();
}

[[nodiscard]] llvm::AllocaInst* createEntryAlloca(llvm::Function*    func,
                                                  const std::string& name,
                                                  llvm::Type* const  type)
{
  return llvm::IRBuilder<>{&func->getEntryBlock(),
                           func->getEntryBlock().begin()}
    .CreateAlloca(type, nullptr, name);
}

[[nodiscard]] llvm::Type* getFloatNTy(CGContext& ctx, const int mantissa_width)
{
  assert(mantissa_width == 32 || mantissa_width == 64);

  return mantissa_width == 32 ? ctx.builder.getFloatTy()
                              : ctx.builder.getDoubleTy();
}

[[nodiscard]] SignKind
logicalOrSign(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return lhs.isSigned(ctx) || rhs.isSigned(ctx) ? SignKind::signed_
                                                : SignKind::unsigned_;
}

// If either of them is signed, the signed type is returned
// Otherwise unsigned
// Assume that the two types (not considering the sign) are the same
[[nodiscard]] std::shared_ptr<Type>
resultIntegerTypeOf(CGContext&                   ctx,
                    const std::shared_ptr<Type>& lhs_t,
                    const std::shared_ptr<Type>& rhs_t)
{
  if (lhs_t->isSigned(ctx))
    return lhs_t;
  else if (rhs_t->isSigned(ctx))
    return rhs_t;

  // If unsigned.
  return lhs_t;
}

[[nodiscard]] Value createAddInverse(CGContext& ctx, const Value& value)
{
  if (value.getValue()->getType()->isFloatingPointTy()) {
    return {ctx.builder.CreateFSub(
              llvm::ConstantFP::getZeroValueForNegation(value.getLLVMType()),
              value.getValue()),
            value.getType()};
  }

  return {ctx.builder.CreateSub(llvm::ConstantInt::get(value.getLLVMType(), 0),
                                value.getValue()),
          value.getType()};
}

[[nodiscard]] Value
createAdd(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFAdd(lhs.getValue(), rhs.getValue()),
            lhs.getType()};
  }

  // Pointer arithmetic
  if (lhs.getType()->isPointerTy(ctx) && rhs.getType()->isIntegerTy(ctx)) {
    return {ctx.builder.CreateInBoundsGEP(
              lhs.getValue()->getType()->getPointerElementType(),
              lhs.getValue(),
              rhs.getValue()),
            lhs.getType()};
  }

  return {ctx.builder.CreateAdd(lhs.getValue(), rhs.getValue()),
          resultIntegerTypeOf(ctx, lhs.getType(), rhs.getType())};
}

[[nodiscard]] Value
createSub(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFSub(lhs.getValue(), rhs.getValue()),
            lhs.getType()};
  }

  // Pointer arithmetic
  if (lhs.getType()->isPointerTy(ctx) && rhs.getType()->isIntegerTy(ctx)) {
    return {ctx.builder.CreateInBoundsGEP(
              lhs.getValue()->getType()->getPointerElementType(),
              lhs.getValue(),
              createAddInverse(ctx, rhs).getValue()),
            lhs.getType()};
  }

  return {ctx.builder.CreateSub(lhs.getValue(), rhs.getValue()),
          resultIntegerTypeOf(ctx, lhs.getType(), rhs.getType())};
}

[[nodiscard]] Value
createMul(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFMul(lhs.getValue(), rhs.getValue()),
            lhs.getType()};
  }

  return {ctx.builder.CreateMul(lhs.getValue(), rhs.getValue()),
          resultIntegerTypeOf(ctx, lhs.getType(), rhs.getType())};
}

[[nodiscard]] Value
createDiv(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFDiv(lhs.getValue(), rhs.getValue()),
            lhs.getType()};
  }

  const auto result_type
    = resultIntegerTypeOf(ctx, lhs.getType(), rhs.getType());

  if (result_type->isSigned(ctx)) {
    return {ctx.builder.CreateSDiv(lhs.getValue(), rhs.getValue()),
            result_type};
  }
  else {
    return {ctx.builder.CreateUDiv(lhs.getValue(), rhs.getValue()),
            result_type};
  }
}

[[nodiscard]] Value
createMod(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFRem(lhs.getValue(), rhs.getValue()),
            lhs.getType()};
  }

  const auto result_type
    = resultIntegerTypeOf(ctx, lhs.getType(), rhs.getType());

  if (result_type->isSigned(ctx)) {
    return {ctx.builder.CreateSRem(lhs.getValue(), rhs.getValue()),
            result_type};
  }
  else {
    return {ctx.builder.CreateURem(lhs.getValue(), rhs.getValue()),
            result_type};
  }
}

[[nodiscard]] Value
createEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::CmpInst::Predicate::FCMP_UEQ,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
  }

  return {ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createNotEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::CmpInst::Predicate::FCMP_UNE,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
  }

  return {ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createLessThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_ULT,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SLT
                                   : llvm::ICmpInst::ICMP_ULT,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createGreaterThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_UGT,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SGT
                                   : llvm::ICmpInst::ICMP_UGT,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createLessOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_ULE,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SLE
                                   : llvm::ICmpInst::ICMP_ULE,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createGreaterOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_UGE,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SGE
                                   : llvm::ICmpInst::ICMP_UGE,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createLogicalAnd(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateLogicalAnd(lhs.getValue(), rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createLogicalOr(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateLogicalOr(lhs.getValue(), rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_, false)};
}

[[nodiscard]] Value
createShiftLeft(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateShl(lhs.getValue(), rhs.getValue()), lhs.getType()};
}

[[nodiscard]] Value
createShiftRight(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateAShr(lhs.getValue(), rhs.getValue()),
          lhs.getType()};
}

[[nodiscard]] Value
createBitwiseAnd(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateAnd(lhs.getValue(), rhs.getValue()), lhs.getType()};
}

[[nodiscard]] Value
createBitwiseOr(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateOr(lhs.getValue(), rhs.getValue()), lhs.getType()};
}

[[nodiscard]] Value
createDereference(CGContext& ctx, const PositionRange& pos, const Value& val)
{
  if (val.getType()->isRefTy(ctx)) {
    return {ctx.builder.CreateLoad(val.getLLVMType()->getPointerElementType(),
                                   val.getValue()),
            val.getType()->getRefeeType(ctx)};
  }

  if (!val.getValue()->getType()->isPointerTy()
      || !val.getType()->isPointerTy(ctx)) {
    throw CodegenError{
      ctx.formatError(pos, "dereference requires pointer operand")};
  }

  return {ctx.builder.CreateLoad(val.getLLVMType()->getPointerElementType(),
                                 val.getValue()),
          val.getType()->getPointeeType(ctx)};
}

[[nodiscard]] Value createDereference(CGContext&                       ctx,
                                      const PositionRange&             pos,
                                      const std::shared_ptr<Variable>& operand)
{
  return createDereference(ctx, pos, operand->getValue(ctx));
}

[[nodiscard]] bool equals(CGContext&                   ctx,
                          const std::shared_ptr<Type>& left,
                          const std::shared_ptr<Type>& right)
{
  return left->getMangledName(ctx) == right->getMangledName(ctx);
}

} // namespace twinkle::codegen
