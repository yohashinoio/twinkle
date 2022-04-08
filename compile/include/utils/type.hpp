/**
 * type.hpp
 *
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

namespace miko
{

enum class BuiltinTypeKind : unsigned char {
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
};

struct Type {
  virtual ~Type() = default;

  virtual BuiltinTypeKind getKind() const noexcept = 0;

  virtual bool isPointer() const noexcept = 0;

  virtual bool isSigned() const noexcept = 0;

  virtual llvm::Type* getType(llvm::IRBuilder<>& builder) const = 0;
};

struct BuiltinType : public Type {
  explicit BuiltinType(const BuiltinTypeKind kind) noexcept
    : kind{kind}
  {
  }

  BuiltinTypeKind getKind() const noexcept override
  {
    return kind;
  }

  bool isPointer() const noexcept override
  {
    return false;
  }

  llvm::Type* getType(llvm::IRBuilder<>& builder) const override
  {
    switch (kind) {
    case BuiltinTypeKind::void_:
      return builder.getVoidTy();
    case BuiltinTypeKind::i8:
    case BuiltinTypeKind::u8:
      return builder.getInt8Ty();
    case BuiltinTypeKind::i16:
    case BuiltinTypeKind::u16:
      return builder.getInt16Ty();
    case BuiltinTypeKind::i32:
    case BuiltinTypeKind::u32:
      return builder.getInt32Ty();
    case BuiltinTypeKind::i64:
    case BuiltinTypeKind::u64:
      return builder.getInt64Ty();
    case BuiltinTypeKind::bool_:
      // We will represent boolean by u8 instead of i1.
      return builder.getInt8Ty();
    }

    llvm_unreachable("");
  }

  bool isSigned() const noexcept override
  {
    switch (kind) {
    case BuiltinTypeKind::i8:
    case BuiltinTypeKind::i16:
    case BuiltinTypeKind::i32:
    case BuiltinTypeKind::i64:
      return true;
    default:
      return false;
    }

    llvm_unreachable("");
  }

  bool isUnsigned() const noexcept
  {
    return !isSigned();
  }

private:
  BuiltinTypeKind kind;
};

struct PointerType : public Type {
  explicit PointerType(const BuiltinType& pointee_type) noexcept
    : pointee_type{pointee_type}
  {
  }

  BuiltinTypeKind getKind() const noexcept override
  {
    return pointee_type.getKind();
  }

  bool isPointer() const noexcept override
  {
    return true;
  }

  llvm::Type* getType(llvm::IRBuilder<>& builder) const override
  {
    return llvm::PointerType::getUnqual(pointee_type.getType(builder));
  }

  bool isSigned() const noexcept override
  {
    return false;
  }

private:
  BuiltinType pointee_type;
};

// Variable qualifier.
enum class VariableQual : unsigned char {
  no_qualifier,
  mutable_,
};

enum class Linkage : unsigned char {
  no_linkage,
  internal,
};

} // namespace miko

#endif
