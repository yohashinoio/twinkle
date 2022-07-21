/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <emera/codegen/type.hpp>
#include <emera/codegen/codegen.hpp>

namespace emera::codegen
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

[[nodiscard]] SignKind BuiltinType::getSignKind(CGContext&) const
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

[[nodiscard]] std::string BuiltinType::getMangledName(CGContext&) const
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

[[nodiscard]] std::shared_ptr<Type>
UserDefinedType::getType(CGContext& ctx) const
{
  {
    const auto type = ctx.class_table[ident];
    if (type)
      return *type;
  }

  {
    const auto type = ctx.alias_table[ident];
    if (type)
      return *type;
  }

  return {}; // Could not find the type
}

[[nodiscard]] llvm::Type* UserDefinedType::getLLVMType(CGContext& ctx) const
{
  const auto type = getType(ctx);

  if (type)
    return type->getLLVMType(ctx);
  else
    return nullptr;
}

[[nodiscard]] std::string UserDefinedType::getMangledName(CGContext& ctx) const
{
  return getType(ctx)->getMangledName(ctx);
}

ClassType::ClassType(CGContext&                    ctx,
                     std::vector<MemberVariable>&& members_arg,
                     const std::u32string&         ident)
  : is_opaque{false}
  , members{std::move(members_arg)}
  , ident{unicode::utf32toUtf8(ident)}
  , type{llvm::StructType::create(ctx.context,
                                  extractTypes(ctx, members),
                                  this->ident)}
{
}

ClassType::ClassType(CGContext&                    ctx,
                     std::vector<MemberVariable>&& members_arg,
                     const std::string&            ident)
  : is_opaque{false}
  , members{std::move(members_arg)}
  , ident{ident}
  , type{
      llvm::StructType::create(ctx.context, extractTypes(ctx, members), ident)}
{
}

ClassType::ClassType(CGContext& ctx, const std::string& ident)
  : is_opaque{true}
  , members{}
  , ident{ident}
  , type{llvm::StructType::create(ctx.context, ident)}
{
}

std::vector<llvm::Type*>
ClassType::extractTypes(CGContext& ctx, const std::vector<MemberVariable>& m)
{
  std::vector<llvm::Type*> retval;

  for (const auto& r : m)
    retval.push_back(r.type->getLLVMType(ctx));

  return retval;
}

void ClassType::setBody(CGContext&                    ctx,
                        std::vector<MemberVariable>&& members_arg) noexcept
{
  llvm::cast<llvm::StructType>(type)->setBody(extractTypes(ctx, members_arg));

  members = std::move(members_arg);
}

[[nodiscard]] std::optional<std::size_t>
ClassType::offsetByName(const std::string_view member_name) const
{
  for (std::size_t offset = 0; const auto& member : members) {
    if (member.name == member_name)
      return offset;
    ++offset;
  }

  return std::nullopt;
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

[[nodiscard]] std::string FunctionType::getMangledName(CGContext& ctx) const
{
  std::ostringstream mangled;

  mangled << "F" << return_type->getMangledName(ctx);

  for (const auto& r : param_types)
    mangled << r->getMangledName(ctx);

  return mangled.str();
}

[[nodiscard]] std::string PointerType::getMangledName(CGContext& ctx) const
{
  return "P" + pointee_type->getMangledName(ctx);
}

[[nodiscard]] std::string ArrayType::getMangledName(CGContext& ctx) const
{
  return "A" + boost::lexical_cast<std::string>(array_size) + "_"
         + element_type->getMangledName(ctx);
}

// Type AST to std::shared_ptr<Type>
struct TypeVisitor : public boost::static_visitor<std::shared_ptr<Type>> {
  [[nodiscard]] std::shared_ptr<Type> operator()(boost::blank) const
  {
    unreachable();
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::BuiltinType& node) const
  {
    return std::make_shared<BuiltinType>(node.kind);
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::ArrayType& node) const
  {
    return std::make_shared<ArrayType>(
      boost::apply_visitor(*this, node.element_type),
      node.size);
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::PointerType& node) const
  {
    return std::make_shared<PointerType>(
      boost::apply_visitor(*this, node.pointee_type));
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::UserDefinedType& node) const
  {
    return std::make_shared<UserDefinedType>(node.name.utf32());
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::ReferenceType& node) const
  {
    return std::make_shared<ReferenceType>(
      boost::apply_visitor(*this, node.refee_type));
  }
};

[[nodiscard]] std::shared_ptr<Type> createType(const ast::Type& ast)
{
  return boost::apply_visitor(TypeVisitor(), ast);
}

} // namespace emera::codegen
