/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/codegen/type.hpp>
#include <twinkle/codegen/codegen.hpp>
#include <twinkle/codegen/common.hpp>
#include <twinkle/codegen/exception.hpp>
#include <twinkle/codegen/top_level.hpp>

namespace twinkle::codegen
{

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
  case BuiltinTypeKind::isize:
  case BuiltinTypeKind::usize:
    return llvm::Type::getIntNTy(
      ctx.context,
      ctx.module->getDataLayout().getMaxIndexSizeInBits());
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
  case BuiltinTypeKind::isize:
    return SignKind::signed_;

  case BuiltinTypeKind::u8:
  case BuiltinTypeKind::u16:
  case BuiltinTypeKind::u32:
  case BuiltinTypeKind::u64:
  case BuiltinTypeKind::bool_:
  case BuiltinTypeKind::char_:
  case BuiltinTypeKind::usize:
    return SignKind::unsigned_;

  case BuiltinTypeKind::void_:
  case BuiltinTypeKind::f64:
  case BuiltinTypeKind::f32:
    return SignKind::no_sign;
  }

  unreachable();
}

[[nodiscard]] bool BuiltinType::isIntegerTy(CGContext&) const
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
  case BuiltinTypeKind::isize:
  case BuiltinTypeKind::usize:
    return true;
  default:
    return false;
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
  case BuiltinTypeKind::isize:
    return "is";
  case BuiltinTypeKind::usize:
    return "us";
  }

  unreachable();
}

[[nodiscard]] std::shared_ptr<Type>
UserDefinedType::getRealType(CGContext& ctx) const
{
  {
    if (const auto type = ctx.class_table[ident])
      return *type;
  }

  {
    if (const auto type = ctx.alias_table[ident])
      return *type;
  }

  {
    if (!ctx.template_argument_tables.empty()) {
      if (const auto type = ctx.template_argument_tables.top()[ident])
        return *type;
    }
  }

  return {}; // Could not find the type
}

[[nodiscard]] llvm::Type* UserDefinedType::getLLVMType(CGContext& ctx) const
{
  const auto type = getRealType(ctx);

  if (type)
    return type->getLLVMType(ctx);
  else
    return nullptr;
}

[[nodiscard]] std::string UserDefinedType::getMangledName(CGContext& ctx) const
{
  return getRealType(ctx)->getMangledName(ctx);
}

llvm::StructType*
ClassType::createStructType(CGContext&                 ctx,
                            std::vector<llvm::Type*>&& members,
                            const std::string&         name)
{
  if (const auto type = llvm::StructType::getTypeByName(ctx.context, name))
    return type;

  return llvm::StructType::create(ctx.context, members, name);
}

ClassType::ClassType(CGContext&                    ctx,
                     std::vector<MemberVariable>&& members_arg,
                     const std::u32string&         u32_name)
  : is_opaque{false}
  , members{std::move(members_arg)}
  , name{unicode::utf32toUtf8(u32_name)}
  , type{createStructType(ctx, extractTypes(ctx, members), name)}
{
}

ClassType::ClassType(CGContext&                    ctx,
                     std::vector<MemberVariable>&& members_arg,
                     const std::string&            name)
  : is_opaque{false}
  , members{std::move(members_arg)}
  , name{name}
  , type{createStructType(ctx, extractTypes(ctx, members), this->name)}
{
}

ClassType::ClassType(CGContext&                    ctx,
                     std::vector<MemberVariable>&& members,
                     const std::string&            ident,
                     llvm::StructType* const       type)
  : is_opaque{true}
  , members{std::move(members)}
  , name{ident}
  , type{type}
{
}

std::shared_ptr<ClassType>
ClassType::createOpaqueClass(CGContext& ctx, const std::string& ident)
{
  return std::make_shared<ClassType>(
    ctx,
    std::vector<MemberVariable>{},
    ident,
    llvm::StructType::create(ctx.context, ident));
}

std::vector<llvm::Type*>
ClassType::extractTypes(CGContext&                         ctx,
                        const std::vector<MemberVariable>& members)
{
  std::vector<llvm::Type*> retval;

  for (const auto& r : members)
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
  TypeVisitor(CGContext& ctx) noexcept
    : ctx{ctx}
  {
  }

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
    assert(0 < node.n_ops.size());

    auto type = boost::apply_visitor(*this, node.pointee_type);

    for (std::size_t i = 0; i < node.n_ops.size(); ++i)
      type = std::make_shared<PointerType>(type);

    return type;
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::UserDefinedType& node) const
  {
    return std::make_shared<UserDefinedType>(node.name.utf32());
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::UserDefinedTemplateType& node) const
  {
    const auto pos = ctx.positions.position_of(node);

    const auto template_type = (*this)(node.template_type);

    const auto class_name
      = template_type->getLLVMType(ctx) ? template_type->getClassName(
          ctx) /* Already defined template type or aliased type */
                                        : node.template_type.name.utf8();

    const auto class_template
      = findClassTemplate(ctx, class_name, node.template_args);

    if (!class_template) {
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("unknown class template '{}'", class_name))};
    }

    auto table_key = CreatedClassTemplateTableKey{class_name,
                                                  node.template_args,
                                                  class_template->second};

    {
      if (const auto type = ctx.created_class_template_table[table_key])
        return *type;
    }

    const auto type = createClassFromTemplate(class_template->first,
                                              node.template_args,
                                              class_template->second,
                                              pos);

    ctx.created_class_template_table.insert(std::move(table_key), type);

    return type;
  }

  [[nodiscard]] std::shared_ptr<Type>
  operator()(const ast::ReferenceType& node) const
  {
    return std::make_shared<ReferenceType>(
      boost::apply_visitor(*this, node.refee_type));
  }

private:
  [[nodiscard]] std::shared_ptr<Type>
  createClassFromTemplate(const ClassTemplateTableValue& ast,
                          const ast::TemplateArguments&  template_args,
                          const NsHierarchy& space, // FIXME: Use this argument
                          const PositionRange& pos) const
  {
    const TemplateArgumentsDefiner ta_definer{ctx,
                                              template_args,
                                              ast.template_params,
                                              pos};

    const auto methods = createClassNoMethodDeclDef(ctx, ast);

    const auto class_name = ast.name.utf8();

    {
      // Save the current insert block because a function template is created
      // from within a function
      const auto return_bb = ctx.builder.GetInsertBlock();

      declareMethods(ctx, methods, class_name);
      defineMethods(ctx, methods, class_name);

      // Return insert point to previous location
      // If null, it means it was called from outside functions, so do nothing
      if (return_bb)
        ctx.builder.SetInsertPoint(return_bb);
    }

    return createType(ctx, ast::UserDefinedType{ast.name}, pos);
  }

  CGContext& ctx;
};

void verifyType(CGContext&                   ctx,
                const std::shared_ptr<Type>& type,
                const PositionRange&         pos)
{
  assert(type);

  if (!type->getLLVMType(ctx))
    throw CodegenError{ctx.formatError(pos, "unknown type name specified")};
}

[[nodiscard]] std::shared_ptr<Type>
createType(CGContext& ctx, const ast::Type& ast, const PositionRange& pos)
{
  const auto type = boost::apply_visitor(TypeVisitor{ctx}, ast);

  verifyType(ctx, type, pos);

  return type;
}

} // namespace twinkle::codegen
