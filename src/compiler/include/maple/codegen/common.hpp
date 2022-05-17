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
  // llvm ir does not allow multiple terminations(e.g. 'ret'),
  // so instead store the return value in the variable below and 'ret' at the
  // end.
  llvm::AllocaInst* return_var;

  // When the return statement is called, store the value in the variable for
  // the return value and move it to the BasicBlock below.
  llvm::BasicBlock* end_bb;

  // Where to go when the break statement is called.
  // nullptr is set if not in a loop
  llvm::BasicBlock* break_bb;

  // Where to go when the continue statement is called.
  // nullptr is set if not in a loop
  llvm::BasicBlock* continue_bb;
};

// Class that wraps llvm::Value.
// Made to handle signs, etc.
struct Value {
  Value(llvm::Value*         value,
        const SignKindStack& sign_info_stack,
        const bool           is_mutable = false);

  Value(llvm::Value*    value,
        SignKindStack&& sign_info_stack,
        const bool      is_mutable = false) noexcept;

  Value()
    : value{nullptr}
  {
  }

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

  // Returns true if the top of the sign information stack is signed.
  [[nodiscard]] bool isSigned() const
  {
    assert(!sign_info_stack.empty());

    const auto top = sign_info_stack.top();
    return top == SignKind::signed_;
  }

  // Returns the top of the sign information stack.
  [[nodiscard]] SignKind getSignKind() const
  {
    assert(!sign_info_stack.empty());

    const auto top = sign_info_stack.top();
    return top;
  }

  [[nodiscard]] const SignKindStack& getSignInfo() const noexcept
  {
    return sign_info_stack;
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

  SignKindStack sign_info_stack;

  bool is_mutable;
};

struct Variable {
  Variable(const Value& alloca, const bool is_mutable) noexcept;

  [[nodiscard]] llvm::AllocaInst* getAllocaInst() const noexcept
  {
    return llvm::cast<llvm::AllocaInst>(alloca.getValue());
  }

  [[nodiscard]] decltype(auto) getSignInfo() const noexcept
  {
    return alloca.getSignInfo();
  }

  [[nodiscard]] bool isMutable() const noexcept
  {
    return is_mutable;
  }

  // If already constant, do nothing.
  void changeToConstant() noexcept
  {
    is_mutable = false;
  }

private:
  Value alloca;
  bool  is_mutable;
};

struct SymbolTable {
  // If the variable does not exist, an out_of_range exception is thrown.
  [[nodiscard]] Variable& operator[](const std::string& name)
  {
    assert(exists(name));
    return named_values.at(name);
  }

  // If the variable already exists, shadow it.
  void registOrShadow(const std::string& name, Variable&& v)
  {
    named_values.insert_or_assign(name, std::move(v));
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

// Create an alloca instruction in the entry block of
// the function.
[[nodiscard]] llvm::AllocaInst* createEntryAlloca(llvm::Function*    func,
                                                  const std::string& var_name,
                                                  llvm::Type*        type);

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
