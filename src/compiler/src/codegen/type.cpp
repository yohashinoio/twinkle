/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/codegen/type.hpp>
#include <maple/codegen/codegen.hpp>
#include <maple/ast/ast.hpp>

namespace maple::codegen
{

[[nodiscard]] std::optional<BuiltinTypeKind>
matchBuildinType(const std::u32string_view type)
{
  static const std::unordered_map<std::u32string_view, BuiltinTypeKind>
    builtin_type_map = {
      {U"void", BuiltinTypeKind::void_},
      {  U"i8",    BuiltinTypeKind::i8},
      {  U"u8",    BuiltinTypeKind::u8},
      { U"i16",   BuiltinTypeKind::i16},
      { U"u16",   BuiltinTypeKind::u16},
      { U"i32",   BuiltinTypeKind::i32},
      { U"u32",   BuiltinTypeKind::u32},
      { U"i64",   BuiltinTypeKind::i64},
      { U"u64",   BuiltinTypeKind::u64},
      {U"bool", BuiltinTypeKind::bool_},
      {U"char", BuiltinTypeKind::char_},
  };

  const auto iter = builtin_type_map.find(type);

  if (iter == builtin_type_map.end())
    return std::nullopt;
  else
    return iter->second;

  unreachable();
}

[[nodiscard]] llvm::Type* BuiltinType::getType(CGContext& ctx) const
{
  switch (kind) {
  case BuiltinTypeKind::void_:
    return llvm::Type::getVoidTy(ctx.context);
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::u8:
    return llvm::IntegerType::getInt8Ty(ctx.context);
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::u16:
    return llvm::IntegerType::getInt16Ty(ctx.context);
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::u32:
    return llvm::IntegerType::getInt32Ty(ctx.context);
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::u64:
    return llvm::IntegerType::getInt64Ty(ctx.context);
  case BuiltinTypeKind::bool_:
    return llvm::IntegerType::getInt1Ty(ctx.context);
  case BuiltinTypeKind::char_:
    return llvm::IntegerType::getInt32Ty(ctx.context);
  }

  unreachable();
}

[[nodiscard]] SignKind BuiltinType::getSignKind() const noexcept
{
  switch (kind) {
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::i64:
    return SignKind::signed_;
  case BuiltinTypeKind::u8:
  case BuiltinTypeKind::u16:
  case BuiltinTypeKind::u32:
  case BuiltinTypeKind::u64:
  case BuiltinTypeKind::bool_:
  case BuiltinTypeKind::char_:
    return SignKind::unsigned_;
  case BuiltinTypeKind::void_:
    return SignKind::no_sign;
  }

  unreachable();
}

[[nodiscard]] llvm::Type* StructType::getType(CGContext& ctx) const
{
  const auto struct_type = ctx.struct_table[ident];
  assert(struct_type);
  return struct_type->second;
}

[[nodiscard]] SignKindStack
StructType::createSignKindStack(CGContext& ctx) const noexcept
{
  const auto struct_type = ctx.struct_table[ident];
  assert(struct_type);

  std::stack<SignKind> tmp;
  tmp.push(SignKind::no_sign); // Struct element type sign.
  tmp.push(SignKind::no_sign); // Struct type sign.

  return tmp;
}

[[nodiscard]] std::string BuiltinType::getMangledName() const
{
  switch (kind) {
  case BuiltinTypeKind::void_:
    return "v";
  case BuiltinTypeKind::i8:
    return "i8";
  case BuiltinTypeKind::i16:
    return "i16";
  case BuiltinTypeKind::i32:
    return "i32";
  case BuiltinTypeKind::i64:
    return "i64";
  case BuiltinTypeKind::u8:
    return "u8";
  case BuiltinTypeKind::u16:
    return "u16";
  case BuiltinTypeKind::u32:
    return "u32";
  case BuiltinTypeKind::u64:
    return "u64";
  case BuiltinTypeKind::bool_:
    return "b";
  case BuiltinTypeKind::char_:
    return "c";
  }

  unreachable();
}

[[nodiscard]] std::string StructType::getMangledName() const
{
  return boost::lexical_cast<std::string>(ident.length()) + ident;
}

[[nodiscard]] std::string PointerType::getMangledName() const
{
  return "P" + pointee_type->getMangledName();
}

[[nodiscard]] std::string ArrayType::getMangledName() const
{
  return "A" + boost::lexical_cast<std::string>(array_size) + "_"
         + element_type->getMangledName();
}

} // namespace maple::codegen
