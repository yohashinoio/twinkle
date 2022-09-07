/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/codegen/top_level.hpp>
#include <twinkle/codegen/stmt.hpp>
#include <twinkle/codegen/exception.hpp>

namespace twinkle::codegen
{

enum class AttrKind {
  unknown,
  nomangle,
};

[[nodiscard]] AttrKind matchAttr(const std::u32string_view attr)
{
  static const std::unordered_map<std::u32string_view, AttrKind> attr_map{
    {U"nomangle", AttrKind::nomangle}
  };

  const auto it = attr_map.find(attr);

  if (it == attr_map.end())
    return AttrKind::unknown;

  return it->second;
}

[[nodiscard]] std::unordered_set<AttrKind>
createAttrKindsFrom(const ast::Attrs& attrs)
{
  std::unordered_set<AttrKind> attr_kinds;

  for (const auto& attr_str : attrs)
    attr_kinds.emplace(matchAttr(attr_str));

  return attr_kinds;
}

// Returns std::nullopt if there are multiple variadic arguments
[[nodiscard]] std::optional<bool>
isVariadicArgs(const ast::ParameterList& params)
{
  bool is_vararg = false;

  for (const auto& r : *params) {
    if (r.is_vararg) {
      if (is_vararg) {
        // Multiple variadic arguments detected.
        return std::nullopt;
      }
      else {
        is_vararg = true;
        continue;
      }
    }
  }

  return is_vararg;
}

[[nodiscard]] std::vector<std::shared_ptr<Type>>
createParamTypes(CGContext&                ctx,
                 const ast::ParameterList& params,
                 const std::size_t         named_params_len)
{
  std::vector<std::shared_ptr<Type>> types(named_params_len);

  const auto pos = ctx.positions.position_of(params);

  for (std::size_t i = 0; i != named_params_len; ++i) {
    const auto& param_type = params->at(i).type;
    types.at(i)            = createType(ctx, param_type, pos);
  }

  return types;
}

[[nodiscard]] std::vector<llvm::Type*>
createLLVMTypes(CGContext& ctx, const std::vector<std::shared_ptr<Type>>& types)
{
  std::vector<llvm::Type*> llvm_types;

  for (const auto& r : types)
    llvm_types.push_back(r->getLLVMType(ctx));

  return llvm_types;
}

[[nodiscard]] static SymbolTable
createArgumentTable(CGContext&                ctx,
                    llvm::Function* const     func,
                    const ast::ParameterList& param_list,
                    llvm::iterator_range<llvm::Function::arg_iterator>&& args)
{
  SymbolTable argument_table;

  const auto pos = ctx.positions.position_of(param_list);

  for (auto& arg : args) {
    const auto& param_node = param_list->at(arg.getArgNo());

    const auto& param_type
      = createType(ctx, param_node.type, ctx.positions.position_of(param_list));

    // Create an alloca for this variable
    auto const alloca = createEntryAlloca(func,
                                          arg.getName().str(),
                                          param_type->getLLVMType(ctx));

    // Store the initial value into the alloca
    ctx.builder.CreateStore(&arg, alloca);

    // Add arguments to variable symbol table
    argument_table.insertOrAssign(
      arg.getName().str(),
      std::make_shared<AllocaVariable>(
        Value{alloca, param_type},
        param_node.qualifier.contains(VariableQual::mutable_)));
  }

  return argument_table;
}

void createFunctionBody(CGContext&                  ctx,
                        llvm::Function* const       func,
                        const std::string_view      name,
                        const ast::ParameterList&   params,
                        const std::shared_ptr<Type> return_type,
                        const ast::Stmt&            body)
{
  auto const entry_bb = llvm::BasicBlock::Create(ctx.context, "", func);
  ctx.builder.SetInsertPoint(entry_bb);

  auto argument_table = createArgumentTable(ctx, func, params, func->args());

  // Used to combine returns into one
  auto const end_bb = llvm::BasicBlock::Create(ctx.context, "end");

  // Return variable
  auto const return_variable
    = return_type->isVoidTy(ctx)
        ? nullptr
        : createEntryAlloca(func, "", return_type->getLLVMType(ctx));

  createStatement(ctx,
                  argument_table,
                  {nullptr, return_variable, end_bb, nullptr, nullptr},
                  body);

  // If there is no return, returns undef
  if (!ctx.builder.GetInsertBlock()->getTerminator()
      && !(return_type->isVoidTy(ctx))) {
    // Return 0 specially for main
    if (name == "main") {
      ctx.builder.CreateStore(
        llvm::ConstantInt::getSigned(func->getReturnType(), 0),
        return_variable);
      ctx.builder.CreateBr(end_bb);
    }
    else {
      ctx.builder.CreateStore(llvm::UndefValue::get(func->getReturnType()),
                              return_variable);
      ctx.builder.CreateBr(end_bb);
    }
  }

  // Inserts a terminator if the function returning void does not have
  // one
  if (return_type->isVoidTy(ctx)
      && !ctx.builder.GetInsertBlock()->getTerminator()) {
    ctx.builder.CreateBr(end_bb);
  }

  // Return
  func->getBasicBlockList().push_back(end_bb);
  ctx.builder.SetInsertPoint(end_bb);

  if (return_variable) {
    auto const retval
      = ctx.builder.CreateLoad(return_variable->getAllocatedType(),
                               return_variable);
    ctx.builder.CreateRet(retval);
  }
  else {
    // Function that returns void
    ctx.builder.CreateRet(nullptr);
  }
}

[[nodiscard]] llvm::Function*
declareFunction(CGContext&                   ctx,
                const ast::FunctionDecl&     node,
                const std::string_view       mangled_name,
                const std::shared_ptr<Type>& return_type)
{
  if (node.params->size() && node.params->at(0).is_vararg) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      "requires a named argument before '...'")};
  }

  const auto is_vararg = isVariadicArgs(node.params);
  if (!is_vararg) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node),
                      "cannot have multiple variable arguments")};
  }

  const auto named_params_len
    = *is_vararg ? node.params->size() - 1 : node.params->size();

  const auto param_types = createParamTypes(ctx, node.params, named_params_len);

  auto const func_type
    = llvm::FunctionType::get(return_type->getLLVMType(ctx),
                              createLLVMTypes(ctx, param_types),
                              *is_vararg);

  auto const func
    = llvm::Function::Create(func_type,
                             llvm::Function::LinkageTypes::ExternalLinkage,
                             mangled_name,
                             *ctx.module);

  ctx.return_type_table.insert(func, return_type);
  ctx.param_types_table.insert(func, std::move(param_types));

  // Set names to all arguments
  for (std::size_t idx = 0; auto&& arg : func->args())
    arg.setName(node.params->at(idx++).name.utf8());

  return func;
}

void verifyConstructor(CGContext&              ctx,
                       const std::string_view  class_name,
                       const ast::Constructor& constructor)
{
  if (class_name != constructor.decl.name.utf8()) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(constructor),
                      "constructor name must be the same as the class name")};
  }
}

void verifyDestructor(CGContext&             ctx,
                      const std::string_view class_name,
                      const ast::Destructor& destructor)
{
  if (class_name != destructor.decl.name.utf8()) {
    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(destructor),
                      "destructor name must be the same as the class name")};
  }

  if (!destructor.decl.params->empty()) {
    throw CodegenError{ctx.formatError(ctx.positions.position_of(destructor),
                                       "destructor does not accept arguments")};
  }
}

void defineMethods(CGContext&          ctx,
                   const ClassMethods& methods,
                   const std::string&  class_name)
{
  ctx.ns_hierarchy.push({class_name, NamespaceKind::class_});

  for (const auto& r : methods)
    createTopLevel(ctx, ast::TopLevel{r});

  ctx.ns_hierarchy.pop();
}

void declareMethods(CGContext&          ctx,
                    const ClassMethods& methods,
                    const std::string&  class_name)
{
  ctx.ns_hierarchy.push({class_name, NamespaceKind::class_});

  for (const auto& r : methods)
    createTopLevel(ctx, ast::TopLevel{r.decl});

  ctx.ns_hierarchy.pop();
}

// Depending on the argument, it can be either declaration only, definition
// only, or declaration and definition
void createMethod(CGContext&             ctx,
                  const ClassMethods&    methods,
                  const std::string&     class_name,
                  const MethodGeneration conv)
{
  switch (conv) {
  case MethodGeneration::define_and_declare:
    // By declaring first, the order of definitions can be ignored
    declareMethods(ctx, methods, class_name);
    defineMethods(ctx, methods, class_name);
    return;
  case MethodGeneration::declare:
    declareMethods(ctx, methods, class_name);
    return;
  case MethodGeneration::define:
    defineMethods(ctx, methods, class_name);
    return;
  }

  unreachable();
}

void createClass(CGContext&             ctx,
                 const ast::ClassDef&   node,
                 const MethodGeneration method_conv)
{
  const auto class_name = node.name.utf8();

  auto accessibility = CLASS_DEFAULT_ACCESSIBILITY;

  std::vector<ClassType::MemberVariable> member_variables;
  ClassMethods                           method_def_asts;

  const auto push_this_ptr = [&](ast::FunctionDecl& decl) {
    decl.params->push_front({ast::Identifier{std::u32string{U"this"}},
                             {VariableQual::mutable_},
                             ast::PointerType{ast::UserDefinedType{node.name}},
                             false});
  };

  // At the end of this function, the classes in this array are deleted
  std::vector<std::string> lazy_delete_classes;

  for (const auto& member : node.members) {
    if (const auto variable
        = boost::get<ast::VariableDefWithoutInit>(&member)) {
      const auto is_mutable
        = variable->qualifier
          && (*variable->qualifier == VariableQual::mutable_);

      const auto type
        = createType(ctx, variable->type, ctx.positions.position_of(*variable));

      member_variables.push_back(
        {variable->name.utf8(), type, is_mutable, accessibility});
    }
    else if (const auto function = boost::get<ast::FunctionDef>(&member)) {
      auto function_clone = *function;

      push_this_ptr(function_clone.decl);

      function_clone.decl.accessibility = accessibility;
      function_clone.is_public          = node.is_public;

      method_def_asts.push_back(std::move(function_clone));
    }
    else if (const auto access_specifier = boost::get<Accessibility>(&member)) {
      switch (*access_specifier) {
      case Accessibility::public_:
        accessibility = Accessibility::public_;
        continue;
      case Accessibility::private_:
        accessibility = Accessibility::private_;
        continue;
      case Accessibility::non_method:
      case Accessibility::unknown:
        unreachable();
      }
    }
    else if (const auto constructor = boost::get<ast::Constructor>(&member)) {
      if (accessibility != Accessibility::public_) {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(*constructor),
                          "constructor must be public")};
      }

      verifyConstructor(ctx, class_name, *constructor);

      auto clone = *constructor;

      clone.decl.is_constructor = true;

      push_this_ptr(clone.decl);

      method_def_asts.push_back(ast::FunctionDef{node.is_public,
                                                 std::move(clone.decl),
                                                 std::move(clone.body)});
    }
    else if (const auto destructor = boost::get<ast::Destructor>(&member)) {
      if (accessibility != Accessibility::public_) {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(*destructor),
                          "destructor must be public")};
      }

      verifyDestructor(ctx, class_name, *destructor);

      auto clone = *destructor;

      clone.decl.is_destructor = true;

      push_this_ptr(clone.decl);

      method_def_asts.push_back(ast::FunctionDef{node.is_public,
                                                 std::move(clone.decl),
                                                 std::move(clone.body)});
    }
    else if (const auto class_ = boost::get<ast::ClassDef>(&member)) {
      if (class_->isTemplate()) {
        // TODO
        unreachable();
      }

      createClass(ctx, *class_, MethodGeneration::define_and_declare);
    }
    else
      unreachable();
  }

  if (const auto opaque_class_ty = ctx.class_table[class_name]) {
    const auto type = opaque_class_ty.value();

    if (!type->isOpaque(ctx)) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        fmt::format("redefinition of '{}'", class_name))};
    }

    type->setBody(ctx, std::move(member_variables));
    type->setIsOpaque(false);
  }
  else {
    ctx.class_table.insert(
      class_name,
      std::make_shared<ClassType>(ctx,
                                  std::move(member_variables),
                                  class_name));
  }

  createMethod(ctx, method_def_asts, class_name, method_conv);

  for (const auto& r : lazy_delete_classes)
    ctx.class_table.erase(r);
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CGContext& ctx, const ast::Attrs& attrs) noexcept
    : ctx{ctx}
    , attr_kinds{createAttrKindsFrom(attrs)}
  {
  }

  llvm::Function* operator()(boost::blank) const
  {
    unreachable();
  }

  llvm::Function* operator()(const ast::FunctionDecl& node) const
  {
    return declareFunction(
      ctx,
      node,
      mangleFunction(node),
      createType(ctx, node.return_type, ctx.positions.position_of(node)));
  }

  llvm::Function* operator()(const ast::FunctionDef& node) const
  {
    if (node.decl.isTemplate()) {
      verifyTemplateParameter(node.decl.template_params);

      const auto name = node.decl.name.utf8();

      const auto key = TemplateTableKey{name,
                                        node.decl.template_params->size(),
                                        ctx.ns_hierarchy};

      if (ctx.func_template_table.exists(key)) {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(node.decl),
                          fmt::format("redefinition of '{}'", name))};
      }

      ctx.func_template_table.insert(key, node);

      return nullptr;
    }

    const auto name = node.decl.name.utf8();

    auto func = ctx.module->getFunction(mangleFunction(node.decl));

    if (func && !func->isDeclaration()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node.decl),
                        fmt::format("redefinition of '{}'", name))};
    }

    if (!func)
      func = (*this)(node.decl);

    assert(func);

    if (!node.is_public && name != "main")
      func->setLinkage(llvm::Function::LinkageTypes::InternalLinkage);

    createFunctionBody(ctx,
                       func,
                       name,
                       node.decl.params,
                       createType(ctx,
                                  node.decl.return_type,
                                  ctx.positions.position_of(node.decl)),
                       node.body);

    ctx.fpm.run(*func);

    return func;
  }

  llvm::Function* operator()(const ast::ClassDecl& node) const
  {
    const auto name = node.name.utf8();

    if (ctx.class_table.exists(name))
      return nullptr;

    ctx.class_table.insert(name, ClassType::createOpaqueClass(ctx, name));

    return nullptr;
  }

  llvm::Function* operator()(const ast::ClassDef& node) const
  {
    if (node.isTemplate()) {
      verifyTemplateParameter(node.template_params);

      const auto name = node.name.utf8();

      const auto key = TemplateTableKey{name,
                                        node.template_params->size(),
                                        ctx.ns_hierarchy};

      if (ctx.class_template_table.exists(key)) {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(node),
                          fmt::format("redefinition of '{}'", name))};
      }

      ctx.class_template_table.insert(key, node);

      return nullptr;
    }

    createClass(ctx, node, MethodGeneration::define_and_declare);

    return nullptr;
  }

  llvm::Function* operator()(const ast::Typedef& node) const
  {
    // TODO: If there is already an alias of the same type, make an error

    ctx.alias_table.insertOrAssign(
      node.alias.utf8(),
      createType(ctx, node.type, ctx.positions.position_of(node)));

    return nullptr;
  }

  llvm::Function* operator()(const ast::Import& node) const
  {
    namespace fs = std::filesystem;

    auto path = ctx.file.parent_path() / fs::path{node.path.utf32()};

    const auto result
      = parse::Parser{loadFile(path, ctx.positions.position_of(node)), path}
          .getResult();

    const auto pos_backup = ctx.positions;
    ctx.positions         = result.positions;

    for (const auto& node_with_attr : result.ast) {
      const auto node = node_with_attr.top_level;

      if (const auto func_def = boost::get<ast::FunctionDef>(&node);
          func_def && func_def->is_public) {
        (*this)(func_def->decl);
        continue;
      }

      if (const auto class_def = boost::get<ast::ClassDef>(&node);
          class_def && class_def->is_public) {
        importClass(*class_def);
        continue;
      }
    }

    ctx.positions = pos_backup;

    return nullptr;
  }

private:
  void verifyTemplateParameter(const ast::TemplateParameters& params) const
  {
    const auto& param_names = params.type_names;

    // Check if template parameters are unique
    for (auto it = param_names.cbegin(), last = param_names.cend(); it != last;
         ++it) {
      for (auto it_c = it + 1; it_c != last; ++it_c) {
        if (*it == *it_c) {
          throw CodegenError{
            ctx.formatError(ctx.positions.position_of(params),
                            fmt::format("redeclaration of '{}'", it->utf8()))};
        }
      }
    }
  }

  void importClass(const ast::ClassDef& node) const
  {
    createClass(ctx, node, MethodGeneration::declare /* Only declaration */);
  }

  [[nodiscard]] std::string loadFile(const std::filesystem::path& path,
                                     const PositionRange&         pos) const
  {
    if (!std::filesystem::exists(path)) {
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("{}: No such file or directory", path.string()))};
    }

    if (auto file = std::ifstream{path, std::ios_base::binary}) {
      std::stringstream ss;
      ss << file.rdbuf();
      return ss.str();
    }

    throw CodegenError{
      ctx.formatError(pos,
                      fmt::format("{}: Could not open file", path.string()))};
  }

  [[nodiscard]] std::string mangleFunction(const ast::FunctionDecl& node) const
  {
    assert(!node.isTemplate());

    const auto name = node.name.utf8();

    if (name == "main" || attr_kinds.contains(AttrKind::nomangle))
      return name;

    return ctx.mangler.mangleFunction(ctx, node);
  }

  CGContext& ctx;

  std::unordered_set<AttrKind> attr_kinds;
};

llvm::Function* createTopLevel(CGContext& ctx, const ast::TopLevel& node)
{
  return boost::apply_visitor(TopLevelVisitor{ctx, {}}, node);
}

llvm::Function* createTopLevel(CGContext&                   ctx,
                               const ast::TopLevelWithAttr& node)
{
  return boost::apply_visitor(TopLevelVisitor{ctx, node.attrs}, node.top_level);
}

} // namespace twinkle::codegen
