/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <rutile/codegen/top_level.hpp>
#include <rutile/codegen/stmt.hpp>
#include <rutile/codegen/exception.hpp>

namespace rutile::codegen
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
[[nodiscard]] static std::optional<bool>
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

[[nodiscard]] static llvm::Function*
createLlvmFunction(const Linkage             linkage,
                   llvm::FunctionType* const type,
                   const llvm::Twine&        name,
                   llvm::Module&             module)
{
  return llvm::Function::Create(type, linkageToLLVM(linkage), name, module);
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CGContext&                         ctx,
                  llvm::legacy::FunctionPassManager& fpm,
                  const ast::Attrs&                  attrs) noexcept
    : ctx{ctx}
    , fpm{fpm}
    , attr_kinds{createAttrKindsFrom(attrs)}
  {
  }

  llvm::Function* operator()(boost::blank) const
  {
    unreachable();
  }

  llvm::Function* operator()(const ast::FunctionDecl& node) const
  {
    const auto name = node.name.utf8();

    const auto return_type = createType(node.return_type);

    if (name.length() == 4 /* For optimization */ && name == "main"
        && !return_type->isIntegerTy(ctx)) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "the return type of main must be an integer")};
    }

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

    const auto param_types = createParamTypes(node.params, named_params_len);

    auto const func_type
      = llvm::FunctionType::get(return_type->getLLVMType(ctx),
                                param_types,
                                *is_vararg);

    const auto mangled_name = mangleFunction(node);

    auto const func
      = createLlvmFunction(node.linkage, func_type, mangled_name, *ctx.module);

    ctx.return_type_table.insertOrAssign(func, return_type);

    // Set names to all arguments.
    for (std::size_t idx = 0; auto&& arg : func->args())
      arg.setName(node.params->at(idx++).name.utf8());

    return func;
  }

  llvm::Function* operator()(const ast::FunctionDef& node) const
  {
    const auto name = node.decl.name.utf8();

    auto func = ctx.module->getFunction(mangleFunction(node.decl));

    if (func && !func->isDeclaration()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node.decl),
                        fmt::format("redefinition of '{}'", name))};
    }

    if (!func)
      func = (*this)(node.decl);

    if (!func) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node.decl),
                        fmt::format("failed to create function '{}'", name))};
    }

    createFunctionBody(func,
                       name,
                       node.decl.params,
                       createType(node.decl.return_type),
                       node.body);

    fpm.run(*func);

    return func;
  }

  llvm::Function* operator()(const ast::ClassDecl& node) const
  {
    const auto name = node.name.utf8();

    if (ctx.class_table.exists(name)) {
      // Do nothing, if already exists.
      return nullptr;
    }

    ctx.class_table.insert(name, std::make_shared<ClassType>(ctx, name));

    return nullptr;
  }

  llvm::Function* operator()(const ast::ClassDef& node) const
  {
    const auto class_name = node.name.utf8();

    auto accessibility = STRUCT_DEFAULT_ACCESSIBILITY;

    std::vector<ClassType::MemberVariable> member_variables;
    std::vector<ast::FunctionDef>          method_def_asts;

    const auto pushThisPtr = [&](ast::FunctionDecl& decl) {
      decl.params->push_front(
        {ast::Identifier{std::u32string{U"this"}},
         {VariableQual::mutable_},
         ast::PointerType{ast::UserDefinedType{node.name}},
         false});
    };

    for (const auto& member : node.members) {
      if (const auto variable
          = boost::get<ast::VariableDefWithoutInit>(&member)) {
        const auto type = createType(variable->type);

        member_variables.push_back(
          {variable->name.utf8(), type, accessibility});
      }
      else if (const auto function = boost::get<ast::FunctionDef>(&member)) {
        auto function_clone = *function;

        // Push this pointer to front
        pushThisPtr(function_clone.decl);

        function_clone.decl.accessibility = accessibility;

        method_def_asts.push_back(std::move(function_clone));
      }
      else if (const auto access_specifier
               = boost::get<Accessibility>(&member)) {
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

        verifyConstructor(class_name, *constructor);

        auto clone = *constructor;

        clone.decl.is_constructor = true;

        pushThisPtr(clone.decl);

        method_def_asts.push_back(
          ast::FunctionDef{std::move(clone.decl), std::move(clone.body)});
      }
      else if (const auto destructor = boost::get<ast::Destructor>(&member)) {
        if (accessibility != Accessibility::public_) {
          throw CodegenError{
            ctx.formatError(ctx.positions.position_of(*destructor),
                            "destructor must be public")};
        }

        verifyDestructor(class_name, *destructor);

        auto clone = *destructor;

        clone.decl.is_destructor = true;

        pushThisPtr(clone.decl);

        method_def_asts.push_back(
          ast::FunctionDef{std::move(clone.decl), std::move(clone.body)});
      }
      else
        unreachable();
    }

    // Check to make sure the name does not already exist.
    if (const auto existed_type = ctx.class_table[class_name]) {
      if (existed_type.value()->isOpaque(ctx)) {
        // Set member type if declared forward.
        llvm::cast<llvm::StructType>(existed_type.value()->getLLVMType(ctx))
          ->setBody(ClassType::extractTypes(ctx, member_variables));
      }
      else {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(node),
                          fmt::format("redefinition of '{}'", class_name))};
      }
    }
    else {
      ctx.class_table.insert(
        class_name,
        std::make_shared<ClassType>(ctx,
                                    std::move(member_variables),
                                    class_name));
    }

    {
      // Generate the methods.
      // Because it will result in an error if the structure type is not
      // registered.
      ctx.ns_hierarchy.push({class_name, NamespaceKind::class_});
      for (const auto& r : method_def_asts)
        (*this)(r.decl);
      for (const auto& r : method_def_asts)
        (*this)(r);
      ctx.ns_hierarchy.pop();
    }

    return nullptr;
  }

  llvm::Function* operator()(const ast::Typedef& node) const
  {
    // TODO: If there is already an alias of the same type, make an error

    ctx.alias_table.insertOrAssign(node.alias.utf8(), createType(node.type));

    return nullptr;
  }

private:
  [[nodiscard]] std::string mangleFunction(const ast::FunctionDecl& node) const
  {
    const auto name = node.name.utf8();

    if (name == "main" || attr_kinds.contains(AttrKind::nomangle))
      return name;

    return ctx.mangler.mangleFunction(ctx, node);
  }

  void createFunctionBody(llvm::Function* const       func,
                          const std::string_view      name,
                          const ast::ParameterList&   params,
                          const std::shared_ptr<Type> return_type,
                          const ast::Stmt&            body) const
  {
    auto const entry_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    ctx.builder.SetInsertPoint(entry_bb);

    auto argument_table = createArgumentTable(func, params, func->args());

    // Used to combine returns into one.
    auto const end_bb = llvm::BasicBlock::Create(ctx.context, "end");

    // Return variable.
    auto const return_variable
      = return_type->isVoid(ctx)
          ? nullptr
          : createEntryAlloca(func, "", return_type->getLLVMType(ctx));

    createStatement(ctx,
                    argument_table,
                    {nullptr, return_variable, end_bb, nullptr, nullptr},
                    body);

    // If there is no return, returns undef.
    if (!ctx.builder.GetInsertBlock()->getTerminator()
        && !(return_type->isVoid(ctx))) {
      // Return 0 specially for main.
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
    // one.
    if (return_type->isVoid(ctx)
        && !ctx.builder.GetInsertBlock()->getTerminator()) {
      ctx.builder.CreateBr(end_bb);
    }

    // Return.
    func->getBasicBlockList().push_back(end_bb);
    ctx.builder.SetInsertPoint(end_bb);

    if (return_variable) {
      auto const retval
        = ctx.builder.CreateLoad(return_variable->getAllocatedType(),
                                 return_variable);
      ctx.builder.CreateRet(retval);
    }
    else {
      // Function that returns void.
      ctx.builder.CreateRet(nullptr);
    }
  }

  [[nodiscard]] SymbolTable createArgumentTable(
    llvm::Function* const                                func,
    const ast::ParameterList&                            param_list,
    llvm::iterator_range<llvm::Function::arg_iterator>&& args) const
  {
    SymbolTable argument_table;

    for (auto& arg : args) {
      const auto& param_node = param_list->at(arg.getArgNo());

      const auto& param_type = createType(param_node.type);

      // Create an alloca for this variable.
      auto const alloca = createEntryAlloca(func,
                                            arg.getName().str(),
                                            param_type->getLLVMType(ctx));

      // Store the initial value into the alloca.
      ctx.builder.CreateStore(&arg, alloca);

      // Add arguments to variable symbol table.
      argument_table.insertOrAssign(
        arg.getName().str(),
        Variable{
          {alloca, param_type},
          param_node.qualifier.contains(VariableQual::mutable_)
      });
    }

    return argument_table;
  }

  [[nodiscard]] std::vector<llvm::Type*>
  createParamTypes(const ast::ParameterList& params,
                   const std::size_t         named_params_len) const
  {
    std::vector<llvm::Type*> types(named_params_len);

    for (std::size_t i = 0; i != named_params_len; ++i) {
      const auto& param_type = params->at(i).type;
      types.at(i)            = createType(param_type)->getLLVMType(ctx);
    }

    return types;
  }

  void verifyConstructor(const std::string_view  class_name,
                         const ast::Constructor& constructor) const
  {
    if (class_name != constructor.decl.name.utf8()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(constructor),
                        "constructor name must be the same as the class name")};
    }
  }

  void verifyDestructor(const std::string_view class_name,
                        const ast::Destructor& destructor) const
  {
    if (class_name != destructor.decl.name.utf8()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(destructor),
                        "destructor name must be the same as the class name")};
    }

    if (!destructor.decl.params->empty()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(destructor),
                        "destructor does not accept arguments")};
    }
  }

  CGContext& ctx;

  llvm::legacy::FunctionPassManager& fpm;

  std::unordered_set<AttrKind> attr_kinds;
};

llvm::Function* createTopLevel(CGContext&                         ctx,
                               llvm::legacy::FunctionPassManager& fpm,
                               const ast::TopLevelWithAttr&       node)
{
  return boost::apply_visitor(TopLevelVisitor{ctx, fpm, node.attrs},
                              node.top_level);
}

} // namespace rutile::codegen
