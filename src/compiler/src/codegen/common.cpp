/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <spica/codegen/common.hpp>

namespace spica::codegen
{

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

// If either of them is signed, the signed type is returned. Otherwise,
// unsigned.
// Assume that the two types (not considering the sign) are the same.
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
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  return {ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createNotEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::CmpInst::Predicate::FCMP_UNE,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  return {ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createLessThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_ULT,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SLT
                                   : llvm::ICmpInst::ICMP_ULT,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createGreaterThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_UGT,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SGT
                                   : llvm::ICmpInst::ICMP_UGT,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createLessOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_ULE,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SLE
                                   : llvm::ICmpInst::ICMP_ULE,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createGreaterOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  if (lhs.getType()->isFloatingPointTy(ctx)) {
    return {ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_UGE,
                                   lhs.getValue(),
                                   rhs.getValue()),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  return {ctx.builder.CreateICmp(isSigned(logicalOrSign(ctx, lhs, rhs))
                                   ? llvm::ICmpInst::ICMP_SGE
                                   : llvm::ICmpInst::ICMP_UGE,
                                 lhs.getValue(),
                                 rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createLogicalAnd(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateLogicalAnd(lhs.getValue(), rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
}

[[nodiscard]] Value
createLogicalOr(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateLogicalOr(lhs.getValue(), rhs.getValue()),
          std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
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
createDereference(CGContext&                                  ctx,
                  const boost::iterator_range<InputIterator>& pos,
                  const Value&                                val)
{
  if (val.getType()->isRefTy(ctx)) {
    return {ctx.builder.CreateLoad(val.getLLVMType()->getPointerElementType(),
                                   val.getValue()),
            val.getType()->getRefeeType(ctx),
            val.isMutable()};
  }

  if (!val.getValue()->getType()->isPointerTy()
      || !val.getType()->isPointerTy(ctx)) {
    throw CodegenError{
      ctx.formatError(pos, "dereference requires pointer operand")};
  }

  return {ctx.builder.CreateLoad(val.getLLVMType()->getPointerElementType(),
                                 val.getValue()),
          val.getType()->getPointeeType(ctx),
          val.isMutable()};
}

[[nodiscard]] Value
createDereference(CGContext&                                  ctx,
                  const boost::iterator_range<InputIterator>& pos,
                  const Variable&                             operand)
{
  return createDereference(
    ctx,
    pos,
    Value{ctx.builder.CreateLoad(operand.getAllocaInst()->getAllocatedType(),
                                 operand.getAllocaInst()),
          operand.getType(),
          operand.isMutable()});
}

// The code is based on https://gist.github.com/quantumsheep.
// Thank you!
[[nodiscard]] bool strictEquals(const llvm::Type* const left,
                                const llvm::Type* const right)
{
  auto left_ptr  = llvm::dyn_cast<llvm::PointerType>(left);
  auto right_ptr = llvm::dyn_cast<llvm::PointerType>(right);

  if (left != right)
    return false;

  if (left->getTypeID() != right->getTypeID())
    return false;

  switch (left->getTypeID()) {
  case llvm::Type::IntegerTyID:
    return llvm::cast<llvm::IntegerType>(left)->getBitWidth()
           == llvm::cast<llvm::IntegerType>(right)->getBitWidth();

  // left == right would have returned true earlier, because types are
  // uniqued.
  case llvm::Type::VoidTyID:
  case llvm::Type::FloatTyID:
  case llvm::Type::DoubleTyID:
  case llvm::Type::X86_FP80TyID:
  case llvm::Type::FP128TyID:
  case llvm::Type::PPC_FP128TyID:
  case llvm::Type::LabelTyID:
  case llvm::Type::MetadataTyID:
  case llvm::Type::TokenTyID:
    return true;

  case llvm::Type::PointerTyID:
    assert(left_ptr && right_ptr && "Both types must be pointers here.");
    return left_ptr->getAddressSpace() == right_ptr->getAddressSpace();

  case llvm::Type::StructTyID:
  {
    auto left_struct  = llvm::cast<llvm::StructType>(left);
    auto right_struct = llvm::cast<llvm::StructType>(right);

    if (left_struct->getNumElements() != right_struct->getNumElements())
      return false;

    if (left_struct->isPacked() != right_struct->isPacked())
      return false;

    for (unsigned i = 0, e = left_struct->getNumElements(); i != e; ++i) {
      if (!strictEquals(left_struct->getElementType(i),
                        right_struct->getElementType(i)))
        return false;
    }

    return true;
  }

  case llvm::Type::FunctionTyID:
  {
    auto left_function  = llvm::cast<llvm::FunctionType>(left);
    auto right_function = llvm::cast<llvm::FunctionType>(right);

    if (left_function->getNumParams() != right_function->getNumParams())
      return false;

    if (left_function->isVarArg() != right_function->isVarArg())
      return false;

    if (!strictEquals(left_function->getReturnType(),
                      right_function->getReturnType()))
      return false;

    for (unsigned i = 0, e = left_function->getNumParams(); i != e; ++i) {
      if (!strictEquals(left_function->getParamType(i),
                        right_function->getParamType(i)))
        return false;
    }

    return true;
  }

  case llvm::Type::ArrayTyID:
  {
    auto left_sequential  = llvm::cast<llvm::ArrayType>(left);
    auto right_sequential = llvm::cast<llvm::ArrayType>(right);

    if (left_sequential->getNumElements() != right_sequential->getNumElements())
      return false;

    return strictEquals(left_sequential->getElementType(),
                        right_sequential->getElementType());
  }

  // VectorType has not yet been used and is not yet implemented.
  case llvm::Type::FixedVectorTyID:
  case llvm::Type::ScalableVectorTyID:
    unreachable();

  default:
    return false;
  }

  unreachable();
}

} // namespace spica::codegen
