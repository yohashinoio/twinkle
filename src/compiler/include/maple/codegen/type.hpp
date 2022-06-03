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

#include <maple/pch/pch.hpp>
#include <maple/support/utils.hpp>
#include <maple/support/kind.hpp>
#include <maple/unicode/unicode.hpp>
#include <boost/lexical_cast.hpp>

namespace maple::codegen
{

enum class BuiltinTypeKind
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

[[nodiscard]] std::optional<BuiltinTypeKind>
matchBuiltinType(const std::u32string_view type);

// Forward declaration.
struct CGContext;

struct Type {
  virtual ~Type() = default;

  [[nodiscard]] virtual SignKind getSignKind() const noexcept = 0;

  [[nodiscard]] virtual llvm::Type* getLLVMType(CGContext& ctx) const = 0;

  [[nodiscard]] virtual std::string getMangledName() const = 0;

  [[nodiscard]] virtual std::shared_ptr<Type> getPointeeType() const
  {
    unreachable();
  }

  [[nodiscard]] virtual std::shared_ptr<Type> getArrayElementType() const
  {
    unreachable();
  }

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

  [[nodiscard]] bool isSigned() const noexcept
  {
    return getSignKind() == SignKind::signed_;
  }

  [[nodiscard]] bool isUnigned()
  {
    return getSignKind() == SignKind::unsigned_;
  }

  [[nodiscard]] virtual std::uint64_t getArraySize() const noexcept
  {
    unreachable();
  }
};

struct BuiltinType : public Type {
  explicit BuiltinType(const BuiltinTypeKind kind) noexcept
    : kind{kind}
  {
  }

  [[nodiscard]] SignKind getSignKind() const noexcept override;

  [[nodiscard]] bool isVoid() const noexcept override
  {
    return kind == BuiltinTypeKind::void_;
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override;

  [[nodiscard]] std::string getMangledName() const override;

private:
  BuiltinTypeKind kind;
};

struct StructType : public Type {
  explicit StructType(const std::u32string& ident)
    : ident{unicode::utf32toUtf8(ident)}
  {
  }

  [[nodiscard]] SignKind getSignKind() const noexcept override
  {
    return SignKind::no_sign;
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override;

  [[nodiscard]] std::string getMangledName() const override;

private:
  std::string ident;
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

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override
  {
    return llvm::PointerType::getUnqual(pointee_type->getLLVMType(ctx));
  }

  [[nodiscard]] std::shared_ptr<Type> getPointeeType() const override
  {
    return pointee_type;
  }

  [[nodiscard]] std::string getMangledName() const override;

  [[nodiscard]] SignKind getSignKind() const noexcept override
  {
    return SignKind::unsigned_;
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

  [[nodiscard]] std::string getMangledName() const override;

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override
  {
    return llvm::ArrayType::get(element_type->getLLVMType(ctx), array_size);
  }

  [[nodiscard]] std::shared_ptr<Type> getArrayElementType() const override
  {
    return element_type;
  }

  [[nodiscard]] bool isArrayTy() const noexcept override
  {
    return true;
  }

  [[nodiscard]] std::uint64_t getArraySize() const noexcept override
  {
    return array_size;
  }

  [[nodiscard]] SignKind getSignKind() const noexcept override
  {
    return SignKind::no_sign;
  }

private:
  std::shared_ptr<Type> element_type;
  std::uint64_t         array_size;
};

} // namespace maple::codegen

#endif
