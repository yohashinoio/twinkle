/**
 * type.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <utils/type.hpp>

namespace maple
{

[[nodiscard]] llvm::Type* BuiltinType::getType(llvm::LLVMContext& context) const
{
  switch (kind) {
  case BuiltinTypeKind::void_:
    return llvm::Type::getVoidTy(context);
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::u8:
    return llvm::IntegerType::getInt8Ty(context);
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::u16:
    return llvm::IntegerType::getInt16Ty(context);
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::u32:
    return llvm::IntegerType::getInt32Ty(context);
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::u64:
    return llvm::IntegerType::getInt64Ty(context);
  case BuiltinTypeKind::bool_:
    return llvm::IntegerType::getInt1Ty(context);
  case BuiltinTypeKind::char_:
    return llvm::IntegerType::getInt32Ty(context);
  }

  unreachable();
}

[[nodiscard]] bool BuiltinType::isSigned() const noexcept
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

  unreachable();
}

[[nodiscard]] std::string BuiltinType::getName() const
{
  switch (kind) {
  case BuiltinTypeKind::void_:
    return "void";
  case BuiltinTypeKind::i8:
    return "i8";
  case BuiltinTypeKind::u8:
    return "u8";
  case BuiltinTypeKind::i16:
    return "i16";
  case BuiltinTypeKind::u16:
    return "u16";
  case BuiltinTypeKind::i32:
    return "i32";
  case BuiltinTypeKind::u32:
    return "u32";
  case BuiltinTypeKind::i64:
    return "i64";
  case BuiltinTypeKind::u64:
    return "u64";
  case BuiltinTypeKind::bool_:
    return "bool";
  case BuiltinTypeKind::char_:
    return "char";
  }

  unreachable();
}

} // namespace maple
