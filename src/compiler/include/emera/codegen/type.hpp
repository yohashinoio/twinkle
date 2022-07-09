/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _c4aa2bde_b6dc_11ec_b909_0242ac120002
#define _c4aa2bde_b6dc_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <emera/pch/pch.hpp>
#include <emera/support/utils.hpp>
#include <emera/support/kind.hpp>
#include <emera/unicode/unicode.hpp>
#include <boost/lexical_cast.hpp>
#include <emera/ast/ast.hpp>

namespace emera::codegen
{

enum class BuiltinTypeKind {
  void_,
  // Integer types.
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
  // Floating-point types.
  f64,
  f32,
};

[[nodiscard]] std::optional<BuiltinTypeKind>
matchBuiltinType(const std::u32string_view type);

// Forward declaration
struct CGContext;

struct Type {
  virtual ~Type() = default;

  [[nodiscard]] virtual SignKind getSignKind(CGContext&) const = 0;

  [[nodiscard]] virtual llvm::Type* getLLVMType(CGContext&) const = 0;

  [[nodiscard]] virtual std::string getMangledName(CGContext&) const = 0;

  [[nodiscard]] virtual std::shared_ptr<Type> getPointeeType(CGContext&) const
  {
    unreachable();
  }

  [[nodiscard]] virtual std::shared_ptr<Type> getRefeeType(CGContext&) const
  {
    unreachable();
  }

  [[nodiscard]] virtual std::shared_ptr<Type>
  getArrayElementType(CGContext&) const
  {
    unreachable();
  }

  [[nodiscard]] virtual std::uint64_t getArraySize(CGContext&) const
  {
    unreachable();
  }

  [[nodiscard]] virtual std::string getClassName(CGContext&) const
  {
    unreachable();
  }

  [[nodiscard]] virtual std::string getUserDefinedTyName(CGContext&) const
  {
    unreachable();
  }

  [[nodiscard]] virtual bool isVoidTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isIntegerTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isFloatingPointTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isPointerTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isClassTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isOpaque(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isArrayTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] virtual bool isRefTy(CGContext&) const
  {
    return false;
  }

  [[nodiscard]] bool isSigned(CGContext& ctx) const
  {
    return getSignKind(ctx) == SignKind::signed_;
  }

  [[nodiscard]] bool isUnigned(CGContext& ctx) const
  {
    return getSignKind(ctx) == SignKind::unsigned_;
  }
};

struct BuiltinType : public Type {
  explicit BuiltinType(const BuiltinTypeKind kind) noexcept
    : kind{kind}
  {
  }

  [[nodiscard]] SignKind getSignKind(CGContext&) const override;

  [[nodiscard]] bool isVoidTy(CGContext&) const override
  {
    return kind == BuiltinTypeKind::void_;
  }

  [[nodiscard]] bool isFloatingPointTy(CGContext&) const override
  {
    return kind == BuiltinTypeKind::f64 || kind == BuiltinTypeKind::f32;
  }

  [[nodiscard]] bool isIntegerTy(CGContext&) const override
  {
    switch (kind) {
    case BuiltinTypeKind::i8:
    case BuiltinTypeKind::i16:
    case BuiltinTypeKind::i32:
    case BuiltinTypeKind::i64:
    case BuiltinTypeKind::u8:
    case BuiltinTypeKind::u16:
    case BuiltinTypeKind::u32:
    case BuiltinTypeKind::u64:
      return true;
    default:
      return false;
    }

    unreachable();
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override;

  [[nodiscard]] std::string getMangledName(CGContext&) const override;

private:
  const BuiltinTypeKind kind;
};

struct UserDefinedType : public Type {
  explicit UserDefinedType(const std::u32string& ident)
    : ident{unicode::utf32toUtf8(ident)}
  {
  }

  explicit UserDefinedType(const std::string& ident)
    : ident{ident}
  {
  }

  [[nodiscard]] SignKind getSignKind(CGContext& ctx) const override
  {
    return getType(ctx)->getSignKind(ctx);
  }

  [[nodiscard]] std::shared_ptr<Type> getType(CGContext& ctx) const;

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override;

  [[nodiscard]] std::shared_ptr<Type>
  getPointeeType(CGContext& ctx) const override
  {
    assert(isPointerTy(ctx));
    return getType(ctx)->getPointeeType(ctx);
  }

  [[nodiscard]] std::shared_ptr<Type>
  getArrayElementType(CGContext& ctx) const override
  {
    return getType(ctx)->getArrayElementType(ctx);
  }

  [[nodiscard]] std::shared_ptr<Type>
  getRefeeType(CGContext& ctx) const override
  {
    return getType(ctx)->getRefeeType(ctx);
  }

  [[nodiscard]] bool isVoidTy(CGContext& ctx) const override
  {
    return getType(ctx)->isVoidTy(ctx);
  }

  [[nodiscard]] bool isIntegerTy(CGContext& ctx) const override
  {
    return getType(ctx)->isIntegerTy(ctx);
  }

  [[nodiscard]] bool isFloatingPointTy(CGContext& ctx) const override
  {
    return getType(ctx)->isFloatingPointTy(ctx);
  }

  [[nodiscard]] bool isOpaque(CGContext& ctx) const override
  {
    return getType(ctx)->isOpaque(ctx);
  }

  [[nodiscard]] bool isClassTy(CGContext& ctx) const override
  {
    return getType(ctx)->isClassTy(ctx);
  }

  [[nodiscard]] bool isPointerTy(CGContext& ctx) const override
  {
    return getType(ctx)->isPointerTy(ctx);
  }

  [[nodiscard]] bool isArrayTy(CGContext& ctx) const override
  {
    return getType(ctx)->isArrayTy(ctx);
  }

  [[nodiscard]] bool isRefTy(CGContext& ctx) const override
  {
    return getType(ctx)->isRefTy(ctx);
  }

  [[nodiscard]] std::uint64_t getArraySize(CGContext& ctx) const override
  {
    return getType(ctx)->getArraySize(ctx);
  }

  [[nodiscard]] std::string getClassName(CGContext& ctx) const override
  {
    return getType(ctx)->getClassName(ctx);
  }

  [[nodiscard]] std::string getUserDefinedTyName(CGContext&) const override
  {
    return ident;
  }

  [[nodiscard]] std::string getMangledName(CGContext& ctx) const override;

private:
  const std::string ident;
};

struct ClassType : public Type {
  struct MemberVariable {
    std::string           name;
    std::shared_ptr<Type> type;
    Accessibility         accessibility;
  };

  ClassType(CGContext&                    ctx,
            std::vector<MemberVariable>&& members,
            const std::u32string&         ident);

  ClassType(CGContext&                    ctx,
            std::vector<MemberVariable>&& members,
            const std::string&            ident);

  ClassType(CGContext& ctx, const std::string& ident);

  [[nodiscard]] SignKind getSignKind(CGContext&) const override
  {
    return SignKind::no_sign;
  }

  // Calculate the offset of a member variable
  // Return std::nullopt if there is no matching member
  [[nodiscard]] std::optional<std::size_t>
  offsetByName(const std::string_view member_name) const
  {
    for (std::size_t offset = 0; const auto& member : members) {
      if (member.name == member_name)
        return offset;
      ++offset;
    }

    return std::nullopt;
  }

  [[nodiscard]] const MemberVariable&
  getMemberVar(const std::size_t offset) const
  {
    return members.at(offset);
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext&) const override
  {
    return type;
  }

  [[nodiscard]] bool isOpaque(CGContext&) const override
  {
    return is_opaque;
  }

  [[nodiscard]] bool isClassTy(CGContext&) const override
  {
    return true;
  }

  [[nodiscard]] std::string getMangledName(CGContext&) const override
  {
    return boost::lexical_cast<std::string>(ident.length()) + ident;
  }

  [[nodiscard]] std::string getClassName(CGContext&) const override
  {
    return ident;
  }

  [[nodiscard]] std::string getUserDefinedTyName(CGContext&) const override
  {
    return ident;
  }

  static std::vector<llvm::Type*>
  extractTypes(CGContext& ctx, const std::vector<MemberVariable>& m);

private:
  const bool                        is_opaque;
  const std::vector<MemberVariable> members;
  const std::string                 ident;

  // If struct is created multiple times, the name will be duplicated, so create
  // it only once and store it in this variable.
  llvm::StructType* const type;
};

struct FunctionType : public Type {
  FunctionType(const std::shared_ptr<Type>&              return_type,
               const std::vector<std::shared_ptr<Type>>& param_types)
    : return_type{return_type}
    , param_types{param_types}
  {
  }

  FunctionType(const std::shared_ptr<Type>&         return_type,
               std::vector<std::shared_ptr<Type>>&& param_types) noexcept
    : return_type{return_type}
    , param_types{std::move(param_types)}
  {
  }

  [[nodiscard]] SignKind getSignKind(CGContext&) const noexcept override
  {
    return SignKind::no_sign;
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override;

  [[nodiscard]] std::string getMangledName(CGContext& ctx) const override;

private:
  const std::shared_ptr<Type>              return_type;
  const std::vector<std::shared_ptr<Type>> param_types;
};

struct PointerType : public Type {
  explicit PointerType(const std::shared_ptr<Type>& pointee_type)
    : pointee_type{pointee_type}
  {
  }

  [[nodiscard]] bool isPointerTy(CGContext&) const override
  {
    return true;
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override
  {
    return llvm::PointerType::getUnqual(pointee_type->getLLVMType(ctx));
  }

  [[nodiscard]] std::shared_ptr<Type> getPointeeType(CGContext&) const override
  {
    return pointee_type;
  }

  [[nodiscard]] std::string getMangledName(CGContext& ctx) const override;

  [[nodiscard]] SignKind getSignKind(CGContext&) const override
  {
    return SignKind::unsigned_;
  }

private:
  const std::shared_ptr<Type> pointee_type;
};

struct ArrayType : public Type {
  ArrayType(const std::shared_ptr<Type>& element_type,
            const std::uint64_t          array_size)
    : element_type{element_type}
    , array_size{array_size}
  {
  }

  [[nodiscard]] std::string getMangledName(CGContext& ctx) const override;

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override
  {
    return llvm::ArrayType::get(element_type->getLLVMType(ctx), array_size);
  }

  [[nodiscard]] std::shared_ptr<Type>
  getArrayElementType(CGContext&) const override
  {
    return element_type;
  }

  [[nodiscard]] bool isArrayTy(CGContext&) const override
  {
    return true;
  }

  [[nodiscard]] std::uint64_t getArraySize(CGContext&) const override
  {
    return array_size;
  }

  [[nodiscard]] SignKind getSignKind(CGContext&) const override
  {
    return SignKind::no_sign;
  }

private:
  const std::shared_ptr<Type> element_type;
  const std::uint64_t         array_size;
};

// Hold pointer type
// However, implement so that dereferences are not required when referencing
struct ReferenceType : public Type {
  explicit ReferenceType(const std::shared_ptr<Type>& refee_type)
    : refee_type{refee_type}
  {
  }

  [[nodiscard]] bool isRefTy(CGContext&) const override
  {
    return true;
  }

  [[nodiscard]] llvm::Type* getLLVMType(CGContext& ctx) const override
  {
    return llvm::PointerType::getUnqual(refee_type->getLLVMType(ctx));
  }

  [[nodiscard]] std::shared_ptr<Type> getRefeeType(CGContext&) const override
  {
    return refee_type;
  }

  [[nodiscard]] std::string getMangledName(CGContext& ctx) const override
  {
    return "R" + refee_type->getMangledName(ctx);
  }

  [[nodiscard]] SignKind getSignKind(CGContext& ctx) const override
  {
    return refee_type->getSignKind(ctx);
  }

private:
  std::shared_ptr<Type> refee_type;
};

[[nodiscard]] std::shared_ptr<Type> createType(const ast::Type& ast);

} // namespace emera::codegen

#endif
