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
#include <utils/util.hpp>
#include <boost/lexical_cast.hpp>

namespace maple
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

  [[nodiscard]] virtual bool isVoid() const noexcept = 0;

  [[nodiscard]] virtual bool isPointer() const noexcept = 0;

  [[nodiscard]] virtual bool isSigned() const noexcept = 0;

  [[nodiscard]] virtual bool isUnigned()
  {
    return !isSigned();
  }

  [[nodiscard]] virtual unsigned int getArraySize() const noexcept
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

  [[nodiscard]] bool isPointer() const noexcept override
  {
    return false;
  }

  [[nodiscard]] llvm::Type* getType(llvm::LLVMContext& context) const override;

  [[nodiscard]] bool isSigned() const noexcept override;

  [[nodiscard]] std::string getName() const override;

private:
  BuiltinTypeKind kind;
};

struct PointerType : public Type {
  explicit PointerType(std::unique_ptr<Type> pointee_type) noexcept
    : pointee_type{std::move(pointee_type)}
  {
  }

  [[nodiscard]] bool isVoid() const noexcept override
  {
    return false;
  }

  [[nodiscard]] bool isPointer() const noexcept override
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

  [[nodiscard]] std::string getName() const override
  {
    // TODO: recursive pointer (like **p)
    return pointee_type->getName().append(0, '*');
  }

private:
  std::unique_ptr<Type> pointee_type;
};

struct ArrayType : public Type {
  explicit ArrayType(std::unique_ptr<Type> element_type,
                     const std::uint64_t   array_size) noexcept
    : element_type{std::move(element_type)}
    , array_size{array_size}
  {
  }

  [[nodiscard]] llvm::Type*
  getElementKind(llvm::LLVMContext& context) const noexcept
  {
    return element_type->getType(context);
  }

  [[nodiscard]] bool isVoid() const noexcept override
  {
    return false;
  }

  [[nodiscard]] bool isPointer() const noexcept override
  {
    return false;
  }

  [[nodiscard]] llvm::Type* getType(llvm::LLVMContext& context) const override
  {
    return llvm::ArrayType::get(element_type->getType(context), array_size);
  }

  [[nodiscard]] unsigned int getArraySize() const noexcept override
  {
    return array_size;
  }

  [[nodiscard]] bool isSigned() const noexcept override
  {
    return false;
  }

  [[nodiscard]] std::string getName() const override
  {
    return element_type->getName() + '['
           + boost::lexical_cast<std::string>(array_size) + ']';
  }

private:
  std::unique_ptr<Type> element_type;
  std::uint64_t         array_size;
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

} // namespace maple

#endif
