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
matchBuiltinType(const std::u32string_view type)
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
      { U"f64",   BuiltinTypeKind::f64},
      { U"f32",   BuiltinTypeKind::f32},
  };

  const auto iter = builtin_type_map.find(type);

  if (iter == builtin_type_map.end())
    return std::nullopt;
  else
    return iter->second;

  unreachable();
}

[[nodiscard]] llvm::Type* BuiltinType::getLLVMType(CGContext& ctx) const
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
  case BuiltinTypeKind::f64:
    return llvm::Type::getDoubleTy(ctx.context);
  case BuiltinTypeKind::f32:
    return llvm::Type::getFloatTy(ctx.context);
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
  case BuiltinTypeKind::f64:
  case BuiltinTypeKind::f32:
    return SignKind::no_sign;
  }

  unreachable();
}

[[nodiscard]] llvm::Type* StructType::getLLVMType(CGContext& ctx) const
{
  const auto struct_type = ctx.struct_table[ident];
  assert(struct_type);
  return struct_type->second;
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
  case BuiltinTypeKind::f64:
    return "f64";
  case BuiltinTypeKind::f32:
    return "f32";
  }

  unreachable();
}

[[nodiscard]] std::string StructType::getMangledName() const
{
  return boost::lexical_cast<std::string>(ident.length()) + ident;
}

[[nodiscard]] llvm::Type* FunctionType::getLLVMType(CGContext& ctx) const
{
  if (param_types.empty())
    return llvm::FunctionType::get(return_type->getLLVMType(ctx), false);

  std::vector<llvm::Type*> params;
  params.reserve(param_types.size());

  for (const auto& r : param_types)
    params.push_back(r->getLLVMType(ctx));

  return llvm::FunctionType::get(return_type->getLLVMType(ctx), params, false);
}

[[nodiscard]] std::string FunctionType::getMangledName() const
{
  std::ostringstream mangled;

  mangled << "F" << return_type->getMangledName();

  for (const auto& r : param_types)
    mangled << r->getMangledName();

  return mangled.str();
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
