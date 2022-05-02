/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4e85d9d0_bc81_11ec_8422_0242ac120002
#define _4e85d9d0_bc81_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <codegen/codegen.hpp>

namespace maple::codegen
{

struct Variable {
  Variable(llvm::AllocaInst*          alloca,
           const bool                 is_mutable,
           const bool                 is_signed,
           const std::optional<bool>& is_pointer_to_signed
           = std::nullopt) noexcept;

  [[nodiscard]] llvm::AllocaInst* getAllocaInst() const noexcept
  {
    return alloca;
  }

  [[nodiscard]] bool isMutable() const noexcept
  {
    return is_mutable;
  }

  [[nodiscard]] bool isSigned() const noexcept
  {
    return is_signed;
  }

  // If std::nullopt, then value is not a pointer.
  [[nodiscard]] const std::optional<bool>& isPointerToSigned() const noexcept
  {
    return is_pointer_to_signed;
  }

private:
  llvm::AllocaInst* alloca;
  bool              is_mutable;
  bool              is_signed;

  // If std::nullopt, then value is not a pointer.
  std::optional<bool> is_pointer_to_signed;
};

struct SymbolTable {
  [[nodiscard]] std::optional<Variable>
  operator[](const std::string& name) const noexcept;

  // Regist stands for register.
  void regist(const std::string& name, const Variable& info)
  {
    named_values.insert({name, info});
  }

  // Returns true if the variable is already registered, false otherwise.
  [[nodiscard]] bool exists(const std::string& name) const
  {
    return named_values.contains(name);
  }

  [[nodiscard]] auto begin() const noexcept
  {
    return named_values.begin();
  }

  [[nodiscard]] auto end() const noexcept
  {
    return named_values.end();
  }

private:
  std::unordered_map<std::string, Variable> named_values;
};

// Class that wraps llvm::Value.
// Made to handle signs, etc.
struct Value {
  Value(llvm::Value*               value,
        const bool                 is_signed  = false,
        const bool                 is_mutable = false,
        const std::optional<bool>& is_pointer_to_signed
        = std::nullopt) noexcept;

  Value() noexcept = default;

  [[nodiscard]] llvm::Value* getValue() const noexcept
  {
    return value;
  }

  [[nodiscard]] llvm::Type* getType() const
  {
    return value->getType();
  }

  [[nodiscard]] bool isMutable() const noexcept
  {
    return is_mutable;
  }

  [[nodiscard]] bool isSigned() const noexcept
  {
    return is_signed;
  }

  [[nodiscard]] bool isPointer() const
  {
    return value->getType()->isPointerTy();
  }

  [[nodiscard]] bool isInteger() const
  {
    return value->getType()->isIntegerTy();
  }

  // If std::nullopt, then value is not a pointer.
  [[nodiscard]] const std::optional<bool>& isPointerToSigned() const noexcept
  {
    return is_pointer_to_signed;
  }

  [[nodiscard]] explicit operator bool() const noexcept
  {
    return value;
  }

private:
  llvm::Value* value;
  bool         is_mutable;
  bool         is_signed;

  // If std::nullopt, then value is not a pointer.
  std::optional<bool> is_pointer_to_signed;
};

// Create an alloca instruction in the entry block of
// the function.
[[nodiscard]] llvm::AllocaInst* createEntryAlloca(llvm::Function*    func,
                                                  const std::string& var_name,
                                                  llvm::Type*        type);

// Return true if one of them is signed.
[[nodiscard]] bool isEitherSigned(const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createAdd(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createSub(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createMul(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createDiv(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createMod(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createNotEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLessThan(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createGreaterThan(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLessOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createGreaterOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value createLogicalNot(CGContext& ctx, const Value& value);

[[nodiscard]] Value createSizeOf(CGContext& ctx, const Value& value);

[[nodiscard]] Value createAddInverse(CGContext& ctx, const Value& num);

[[nodiscard]] bool strictEquals(const llvm::Type* const left,
                                const llvm::Type* const right);

} // namespace maple::codegen

#endif
