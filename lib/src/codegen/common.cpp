/**
 * common.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/common.hpp>

namespace maple::codegen
{

Variable::Variable(llvm::AllocaInst* pointer,
                   const bool        is_mutable,
                   const bool        is_signed) noexcept
  : pointer{pointer}
  , is_mutable{is_mutable}
  , is_signed{is_signed}
{
}

Value::Value(llvm::Value* value, const bool is_signed) noexcept
  : value{value}
  , is_signed{is_signed}
{
}

Value::Value(llvm::Value* value) noexcept
  : value{value}
  , is_signed{false}
{
}

[[nodiscard]] std::optional<Variable>
SymbolTable::operator[](const std::string& name) const noexcept
try {
  return named_values.at(name);
}
catch (const std::out_of_range&) {
  return std::nullopt;
}

[[nodiscard]] llvm::AllocaInst* createEntryAlloca(llvm::Function*    func,
                                                  const std::string& var_name,
                                                  llvm::Type*        type)
{
  return llvm::IRBuilder<>{&func->getEntryBlock(),
                           func->getEntryBlock().begin()}
    .CreateAlloca(type, nullptr, var_name);
}

[[nodiscard]] bool isEitherSigned(const Value& lhs, const Value& rhs)
{
  return lhs.isSigned() || rhs.isSigned();
}

[[nodiscard]] Value
genAddition(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateAdd(lhs.getValue(), rhs.getValue()),
          isEitherSigned(lhs, rhs)};
}

[[nodiscard]] Value
genSubtraction(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateSub(lhs.getValue(), rhs.getValue()),
          isEitherSigned(lhs, rhs)};
}

[[nodiscard]] Value
genMultiplication(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateMul(lhs.getValue(), rhs.getValue()),
          isEitherSigned(lhs, rhs)};
}

[[nodiscard]] Value
genDivision(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  const auto result_is_signed = isEitherSigned(lhs, rhs);

  if (result_is_signed) {
    return {ctx.builder.CreateSDiv(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }
  else {
    return {ctx.builder.CreateUDiv(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }
}

[[nodiscard]] Value
genModulo(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  const auto result_is_signed = isEitherSigned(lhs, rhs);

  if (result_is_signed) {
    return {ctx.builder.CreateSRem(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }
  else {
    return {ctx.builder.CreateURem(lhs.getValue(), rhs.getValue()),
            result_is_signed};
  }
}

[[nodiscard]] Value genEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                      lhs.getValue(),
                                      rhs.getValue())};
}

[[nodiscard]] Value
genNotEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                      lhs.getValue(),
                                      rhs.getValue())};
}

[[nodiscard]] Value
genLessThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.builder.CreateICmp(isEitherSigned(lhs, rhs)
                                        ? llvm::ICmpInst::ICMP_SLT
                                        : llvm::ICmpInst::ICMP_ULT,
                                      lhs.getValue(),
                                      rhs.getValue())};
}

[[nodiscard]] Value
genGreaterThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.builder.CreateICmp(isEitherSigned(lhs, rhs)
                                        ? llvm::ICmpInst::ICMP_SGT
                                        : llvm::ICmpInst::ICMP_UGT,
                                      lhs.getValue(),
                                      rhs.getValue())};
}

[[nodiscard]] Value
genLessOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.builder.CreateICmp(isEitherSigned(lhs, rhs)
                                        ? llvm::ICmpInst::ICMP_SLE
                                        : llvm::ICmpInst::ICMP_ULE,
                                      lhs.getValue(),
                                      rhs.getValue())};
}

[[nodiscard]] Value
genGreaterOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.builder.CreateICmp(isEitherSigned(lhs, rhs)
                                        ? llvm::ICmpInst::ICMP_SGE
                                        : llvm::ICmpInst::ICMP_UGE,
                                      lhs.getValue(),
                                      rhs.getValue())};
}

[[nodiscard]] Value genLogicalNegative(CGContext& ctx, const Value& value)
{
  return Value{
    ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                           value.getValue(),
                           llvm::ConstantInt::get(value.getType(), 0))};
}

[[nodiscard]] Value inverse(CGContext& ctx, const Value& num)
{
  return Value{ctx.builder.CreateSub(llvm::ConstantInt::get(num.getType(), 0),
                                     num.getValue()),
               num.isSigned()};
}

// The code is based on https://gist.github.com/quantumsheep.
// Thank you!
[[nodiscard]] bool equals(const llvm::Type* const left,
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

  // left == right would have returned true earlier, because types are uniqued.
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
      if (!equals(left_struct->getElementType(i),
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

    if (!equals(left_function->getReturnType(),
                right_function->getReturnType()))
      return false;

    for (unsigned i = 0, e = left_function->getNumParams(); i != e; ++i) {
      if (!equals(left_function->getParamType(i),
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

    return equals(left_sequential->getElementType(),
                  right_sequential->getElementType());
  }

    // TODO: add VectorType case

  default:
    return false;
  }

  unreachable();
}

} // namespace maple::codegen
