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

[[nodiscard]] bool eitherSigned(const Value& lhs, const Value& rhs)
{
  return lhs.isSigned() || rhs.isSigned();
}

[[nodiscard]] Value
genAddition(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateAdd(lhs.getValue(), rhs.getValue()),
          eitherSigned(lhs, rhs)};
}

[[nodiscard]] Value
genSubtraction(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateSub(lhs.getValue(), rhs.getValue()),
          eitherSigned(lhs, rhs)};
}

[[nodiscard]] Value
genMultiplication(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return {ctx.builder.CreateMul(lhs.getValue(), rhs.getValue()),
          eitherSigned(lhs, rhs)};
}

[[nodiscard]] Value
genDivision(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  const auto result_is_signed = eitherSigned(lhs, rhs);

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
  const auto result_is_signed = eitherSigned(lhs, rhs);

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
  return Value{ctx.int1ToBool(ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                                     lhs.getValue(),
                                                     rhs.getValue()))};
}

[[nodiscard]] Value
genNotEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.int1ToBool(ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                                     lhs.getValue(),
                                                     rhs.getValue()))};
}

[[nodiscard]] Value
genLessThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.int1ToBool(
    ctx.builder.CreateICmp(eitherSigned(lhs, rhs) ? llvm::ICmpInst::ICMP_SLT
                                                  : llvm::ICmpInst::ICMP_ULT,
                           lhs.getValue(),
                           rhs.getValue()))};
}

[[nodiscard]] Value
genGreaterThan(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.int1ToBool(
    ctx.builder.CreateICmp(eitherSigned(lhs, rhs) ? llvm::ICmpInst::ICMP_SGT
                                                  : llvm::ICmpInst::ICMP_UGT,
                           lhs.getValue(),
                           rhs.getValue()))};
}

[[nodiscard]] Value
genLessOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.int1ToBool(
    ctx.builder.CreateICmp(eitherSigned(lhs, rhs) ? llvm::ICmpInst::ICMP_SLE
                                                  : llvm::ICmpInst::ICMP_ULE,
                           lhs.getValue(),
                           rhs.getValue()))};
}

[[nodiscard]] Value
genGreaterOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs)
{
  return Value{ctx.int1ToBool(
    ctx.builder.CreateICmp(eitherSigned(lhs, rhs) ? llvm::ICmpInst::ICMP_SGE
                                                  : llvm::ICmpInst::ICMP_UGE,
                           lhs.getValue(),
                           rhs.getValue()))};
}

[[nodiscard]] Value inverse(CGContext& ctx, const Value& num)
{
  return Value{
    ctx.builder.CreateSub(llvm::ConstantInt::get(num.getValue()->getType(), 0),
                          num.getValue()),
    num.isSigned()};
}

} // namespace maple::codegen
