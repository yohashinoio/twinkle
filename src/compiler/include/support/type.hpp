/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _c4aa2bde_b6dc_11ec_b909_0242ac120002
#define _c4aa2bde_b6dc_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <support/utils.hpp>
#include <boost/lexical_cast.hpp>

namespace maple
{

enum class SignKind : unsigned char
{
  unsigned_,
  signed_,
};

[[nodiscard]] inline bool isSigned(const SignKind sk) noexcept
{
  return sk == SignKind::signed_;
}

/*
  |--------------------|
  |        Type | Size |
  |--------------------|
  | Non-pointer |    1 |
  |     Pointer |    2 |
  |  Double ptr |    3 |
  |  Triple ptr |    4 |
  |         ... |  ... |
  |--------------------|

  Example: i32
  |-------------------------|
  |       signed (i32 type) | <- top
  |-------------------------|

  Example: *i32
  |-------------------------|
  | unsigned (pointer type) | <- top
  |       signed (i32 type) |
  |-------------------------|

  Example: **i32
  |-------------------------|
  | unsigned (pointer type) | <- top
  | unsigned (pointer type) |
  |       signed (i32 type) |
  |-------------------------|

  Example: u32[]
  |-------------------------|
  |   unsigned (array type) | <- top
  |     unsigned (u32 type) |
  |-------------------------|

  Example: *i32[]
  |-------------------------|
  |   unsigned (array type) | <- top
  | unsigned (pointer type) |
  |       signed (i32 type) |
  |-------------------------|
*/
using SignKindStack = std::stack<SignKind>;

enum class BuiltinTypeKind : unsigned char
{
  void_,
  i8,
  i16,
  i32,
  i64,
  u8,
  u16,
  u32,
  u64,
  bool_,
  char_,
};

struct Type {
  virtual ~Type() = default;

  [[nodiscard]] virtual bool isVoid() const noexcept
  {
    return false;
  }

  [[nodiscard]] virtual bool isArrayTy() const noexcept
  {
    return false;
  }

  [[nodiscard]] virtual bool isPointerTy() const noexcept
  {
    return false;
  }

  [[nodiscard]] virtual bool isSigned() const noexcept = 0;

  [[nodiscard]] bool isUnigned()
  {
    return !isSigned();
  }

  [[nodiscard]] SignKind getSignKind() const noexcept
  {
    return isSigned() ? SignKind::signed_ : SignKind::unsigned_;
  }

  [[nodiscard]] virtual SignKindStack createSignKindStack() const noexcept = 0;

  [[nodiscard]] virtual std::uint64_t getArraySize() const noexcept
  {
    unreachable();
  }

  [[nodiscard]] virtual std::string getName() const = 0;

  [[nodiscard]] virtual llvm::Type*
  getType(llvm::LLVMContext& context) const = 0;
};

struct BuiltinType : public Type {
  explicit BuiltinType(const BuiltinTypeKind kind) noexcept
    : kind{kind}
  {
  }

  [[nodiscard]] bool isVoid() const noexcept override
  {
    return kind == BuiltinTypeKind::void_;
  }

  [[nodiscard]] llvm::Type* getType(llvm::LLVMContext& context) const override;

  [[nodiscard]] bool isSigned() const noexcept override;

  /*
    Example: i32
    |-------------------|
    | signed (i32 type) | <- top
    |-------------------|
  */
  [[nodiscard]] SignKindStack createSignKindStack() const noexcept override
  {
    return createStack(getSignKind());
  }

  [[nodiscard]] std::string getName() const override;

private:
  BuiltinTypeKind kind;
};

struct PointerType : public Type {
  explicit PointerType(std::shared_ptr<Type> pointee_type) noexcept
    : pointee_type{pointee_type}
  {
  }

  [[nodiscard]] bool isPointerTy() const noexcept override
  {
    return true;
  }

  [[nodiscard]] llvm::Type* getType(llvm::LLVMContext& context) const override
  {
    return llvm::PointerType::getUnqual(pointee_type->getType(context));
  }

  [[nodiscard]] bool isSigned() const noexcept override
  {
    return false;
  }

  /*
    Example: *i32
    |-------------------------|
    | unsigned (pointer type) | <- top
    |       signed (i32 type) |
    |-------------------------|

    Example: **i32
    |-------------------------|
    | unsigned (pointer type) | <- top
    | unsigned (pointer type) |
    |       signed (i32 type) |
    |-------------------------|
  */
  [[nodiscard]] SignKindStack createSignKindStack() const noexcept override
  {
    auto tmp = pointee_type->createSignKindStack();
    tmp.emplace(getSignKind());
    return tmp;
  }

  [[nodiscard]] std::string getName() const override
  {
    // TODO: recursive pointer (like **p)
    return fmt::format("*{}", pointee_type->getName());
  }

private:
  std::shared_ptr<Type> pointee_type;
};

struct ArrayType : public Type {
  ArrayType(std::shared_ptr<Type> element_type,
            const std::uint64_t   array_size) noexcept
    : element_type{element_type}
    , array_size{array_size}
  {
  }

  [[nodiscard]] llvm::Type* getType(llvm::LLVMContext& context) const override
  {
    return llvm::ArrayType::get(element_type->getType(context), array_size);
  }

  [[nodiscard]] bool isArrayTy() const noexcept override
  {
    return true;
  }

  [[nodiscard]] std::uint64_t getArraySize() const noexcept override
  {
    return array_size;
  }

  [[nodiscard]] bool isSigned() const noexcept override
  {
    return false;
  }

  /*
    Example: u32[]
    |-------------------------|
    |   unsigned (array type) | <- top
    |     unsigned (u32 type) |
    |-------------------------|

    Example: *i32[]
    |-------------------------|
    |   unsigned (array type) | <- top
    | unsigned (pointer type) |
    |       signed (i32 type) |
    |-------------------------|
  */
  [[nodiscard]] SignKindStack createSignKindStack() const noexcept override
  {
    auto tmp = element_type->createSignKindStack();
    tmp.emplace(getSignKind());
    return tmp;
  }

  [[nodiscard]] std::string getName() const override
  {
    return element_type->getName() + '['
           + boost::lexical_cast<std::string>(array_size) + ']';
  }

private:
  std::shared_ptr<Type> element_type;
  std::uint64_t         array_size;
};

// Variable qualifier.
enum class VariableQual : unsigned char
{
  no_qualifier,
  mutable_,
};

enum class Linkage : unsigned char
{
  no_linkage,
  internal,
};

} // namespace maple

#endif
