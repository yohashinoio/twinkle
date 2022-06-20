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

#include <maple/pch/pch.hpp>
#include <maple/codegen/codegen.hpp>

namespace maple::codegen
{

struct StmtContext {
  // Store the return value in this variable
  llvm::AllocaInst* return_var;

  // All ret instructions in llvm ir are executed in this basic block
  llvm::BasicBlock* end_bb;

  // Used when a break statement is called
  // If not in a loop, nullptr
  llvm::BasicBlock* break_bb;

  // Used when a continue statement is called
  // If not in a loop, nullptr
  llvm::BasicBlock* continue_bb;
};

// Class that wraps llvm::Value.
// Made to handle signs, etc.
struct Value {
  Value(llvm::Value*                 value,
        const std::shared_ptr<Type>& type,
        const bool                   is_mutable = false)
    : value{value}
    , type{type}
    , is_mutable{is_mutable}
  {
  }

  Value()
    : value{}
  {
  }

  [[nodiscard]] llvm::Value* getValue() const noexcept
  {
    return value;
  }

  [[nodiscard]] llvm::Type* getLLVMType() const
  {
    return value->getType();
  }

  [[nodiscard]] std::shared_ptr<Type> getType() const
  {
    return type;
  }

  [[nodiscard]] bool isMutable() const noexcept
  {
    return is_mutable;
  }

  [[nodiscard]] bool isSigned() const noexcept
  {
    return type->isSigned();
  }

  [[nodiscard]] bool isPointer() const
  {
    return value->getType()->isPointerTy();
  }

  [[nodiscard]] bool isInteger() const
  {
    return value->getType()->isIntegerTy();
  }

  [[nodiscard]] explicit operator bool() const noexcept
  {
    return value;
  }

private:
  llvm::Value* value;

  std::shared_ptr<Type> type;

  bool is_mutable;
};

struct Variable {
  Variable(const Value& alloca, const bool is_mutable) noexcept
    : alloca{alloca}
    , is_mutable{is_mutable}
  {
    assert(llvm::dyn_cast<llvm::AllocaInst>(alloca.getValue()));
  }

  [[nodiscard]] llvm::AllocaInst* getAllocaInst() const noexcept
  {
    return llvm::cast<llvm::AllocaInst>(alloca.getValue());
  }

  [[nodiscard]] std::shared_ptr<Type> getType() const
  {
    return alloca.getType();
  }

  [[nodiscard]] bool isSigned() const noexcept
  {
    return alloca.isSigned();
  }

  [[nodiscard]] bool isMutable() const noexcept
  {
    return is_mutable;
  }

private:
  Value alloca;

  bool is_mutable;
};

using SymbolTable
  = Table<std::string, Variable, std::reference_wrapper<const Variable>>;

// Create an alloca instruction in the entry block of
// the function.
[[nodiscard]] llvm::AllocaInst* createEntryAlloca(llvm::Function*    func,
                                                  const std::string& var_name,
                                                  llvm::Type*        type);

[[nodiscard]] llvm::Type* getFloatNTy(CGContext& ctx, const int mantissa_width);

/*
  |--------------------------------|
  |     Left |    Right |   Result |
  |----------|----------|----------|
  |   signed |   signed |   signed |
  |   signed | unsigned |   signed |
  | unsigned |   signed |   signed |
  | unsigned | unsigned | unsigned |
  |--------------------------------|
*/
[[nodiscard]] SignKind logicalOrSign(const Value& lhs, const Value& rhs);

// If either of them is signed, the signed type is returned. Otherwise,
// unsigned.
// Assuming the type is the same.
[[nodiscard]] std::shared_ptr<Type>
resultIntegerTypeOf(const std::shared_ptr<Type>& lhs_t,
                    const std::shared_ptr<Type>& rhs_t);

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

[[nodiscard]] Value
createLogicalAnd(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLogicalOr(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value createLogicalNot(CGContext& ctx, const Value& value);

[[nodiscard]] Value createSizeOf(CGContext& ctx, const Value& value);

[[nodiscard]] Value createAddInverse(CGContext& ctx, const Value& num);

[[nodiscard]] bool strictEquals(const llvm::Type* const left,
                                const llvm::Type* const right);

} // namespace maple::codegen

#endif
