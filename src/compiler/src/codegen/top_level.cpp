/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/codegen/top_level.hpp>
#include <maple/codegen/stmt.hpp>
#include <maple/codegen/exception.hpp>
#include <unordered_set>

namespace maple::codegen
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
                  llvm::legacy::FunctionPassManager& fp_manager,
                  const ast::Attrs&                  attrs) noexcept
    : ctx{ctx}
    , fp_manager{fp_manager}
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

    if (name.length() == 4 /* For optimization */ && name == "main"
        && !node.return_type->isIntegerTy()) {
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
      = llvm::FunctionType::get(node.return_type->getLLVMType(ctx),
                                param_types,
                                *is_vararg);

    const auto mangled_name = [&]() {
      // main function does not mangle.
      if (name == "main" || attr_kinds.contains(AttrKind::nomangle))
        return name;
      return ctx.mangler.mangleFunction(ctx, node);
    }();

    auto const func
      = createLlvmFunction(node.linkage, func_type, mangled_name, *ctx.module);

    ctx.return_type_table.registOrOverwrite(func, node.return_type);

    // Set names to all arguments.
    for (std::size_t idx = 0; auto&& arg : func->args())
      arg.setName(node.params->at(idx++).name.utf8());

    return func;
  }

  llvm::Function* operator()(const ast::FunctionDef& node) const
  {
    const auto name = node.decl.name.utf8();

    auto func
      = ctx.module->getFunction(ctx.mangler.mangleFunction(ctx, node.decl));

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

    auto const entry_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    ctx.builder.SetInsertPoint(entry_bb);

    auto argument_table
      = createArgumentTable(func, node.decl.params, func->args());

    // Used to combine returns into one.
    auto const end_bb = llvm::BasicBlock::Create(ctx.context);
    // Return variable.
    auto const return_variable
      = node.decl.return_type->isVoid()
          ? nullptr
          : createEntryAlloca(func,
                              "",
                              node.decl.return_type->getLLVMType(ctx));

    createStatement(ctx,
                    argument_table,
                    {return_variable, end_bb, nullptr, nullptr},
                    node.body);

    // If there is no return, returns undef.
    if (!ctx.builder.GetInsertBlock()->getTerminator()
        && !(node.decl.return_type->isVoid())) {
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
    if (node.decl.return_type->isVoid()
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

    fp_manager.run(*func);

    return func;
  }

  llvm::Function* operator()(const ast::StructDecl& node) const
  {
    const auto name = node.name.utf8();

    if (ctx.struct_table.exists(name)) {
      // Do nothing, if already exists.
      return nullptr;
    }

    ctx.struct_table.regist(
      name,
      std::make_pair(std::nullopt, /* std::nullopt means opaque */
                     llvm::StructType::create(ctx.context, name)));

    return nullptr;
  }

  llvm::Function* operator()(const ast::StructDef& node) const
  {
    const auto name = node.name.utf8();

    // Set element types and create element sign info.
    std::vector<llvm::Type*>                 element_types;
    std::vector<ast::VariableDefWithoutInit> member_variables;
    std::vector<ast::FunctionDef>            methods;
    for (const auto& element : node.elements) {
      if (const auto* variable
          = boost::get<ast::VariableDefWithoutInit>(&element)) {
        // Member variables
        element_types.emplace_back(variable->type->getLLVMType(ctx));
        member_variables.push_back(*variable);
        continue;
      }

      if (const auto* function = boost::get<ast::FunctionDef>(&element)) {
        // Methods
        auto function_clone = *function;

        // Push 'this*'
        function_clone.decl.params->push_front(
          {ast::Identifier{std::u32string{U"this"}},
           VariableQual::mutable_,
           std::make_shared<PointerType>(
             std::make_shared<StructType>(node.name.utf32())),
           false});

        methods.push_back(std::move(function_clone));
        continue;
      }

      unreachable();
    }

    // Check to make sure the name does not already exist.
    if (const auto existed_type = ctx.struct_table[name]) {
      if (existed_type->second->isOpaque()) {
        // Set element type if declared forward.
        existed_type->second->setBody(element_types);
      }
      else {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(node),
                          fmt::format("redefinition of '{}'", name))};
      }
    }
    else {
      ctx.struct_table.regist(
        name,
        std::make_pair(
          std::move(member_variables),
          llvm::StructType::create(ctx.context, element_types, name)));
    }

    {
      // Generate the methods.
      // Because it will result in an error if the structure type is not
      // registered.
      ctx.namespaces.push({name, true});
      for (const auto& r : methods)
        (*this)(r);
      ctx.namespaces.pop();
    }

    return nullptr;
  }

private:
  SymbolTable createArgumentTable(
    llvm::Function* const                                func,
    const ast::ParameterList&                            param_list,
    llvm::iterator_range<llvm::Function::arg_iterator>&& args) const
  {
    SymbolTable argument_table;

    for (auto& arg : args) {
      const auto& param_node = param_list->at(arg.getArgNo());

      // Create an alloca for this variable.
      auto const alloca = createEntryAlloca(func,
                                            arg.getName().str(),
                                            param_node.type->getLLVMType(ctx));

      // Store the initial value into the alloca.
      ctx.builder.CreateStore(&arg, alloca);

      const auto is_mutable
        = param_node.qualifier
          && (*param_node.qualifier == VariableQual::mutable_);

      // Add arguments to variable symbol table.
      argument_table.registOrOverwrite(arg.getName().str(),
                                       Variable{
                                         {alloca, param_node.type},
                                         is_mutable
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
      types.at(i)            = param_type->getLLVMType(ctx);
    }

    return types;
  }

  CGContext& ctx;

  llvm::legacy::FunctionPassManager& fp_manager;

  std::unordered_set<AttrKind> attr_kinds;
};

llvm::Function* createTopLevel(CGContext&                         ctx,
                               llvm::legacy::FunctionPassManager& fp_manager,
                               const ast::TopLevelWithAttr&       node)
{
  return boost::apply_visitor(TopLevelVisitor{ctx, fp_manager, node.attrs},
                              node.top_level);
}

} // namespace maple::codegen
