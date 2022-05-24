/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/codegen/top_level.hpp>
#include <maple/codegen/stmt.hpp>
#include <maple/codegen/exception.hpp>

namespace maple::codegen
{

// Returns std::nullopt if there are multiple variadic arguments
[[nodiscard]] static std::optional<bool>
isVariadicArgs(const std::vector<ast::Parameter>& params)
{
  bool is_variadic_args = false;

  for (const auto& r : params) {
    if (r.is_variadic_args) {
      if (is_variadic_args) {
        // Multiple variadic arguments detected.
        return std::nullopt;
      }
      else {
        is_variadic_args = true;
        continue;
      }
    }
  }

  return is_variadic_args;
}

[[nodiscard]] static llvm::Function*
createLlvmFunction(const Linkage             linkage,
                   llvm::FunctionType* const type,
                   const llvm::Twine&        name,
                   llvm::Module&             module)
{
  return llvm::Function::Create(type, linkageToLLVM(linkage), name, module);
}

SymbolTable
createArgumentTable(CGContext&                ctx,
                    llvm::Function* const     func,
                    const ast::ParameterList& param_list,
                    llvm::iterator_range<llvm::Function::arg_iterator>&& args)
{
  SymbolTable argument_table;

  for (auto& arg : args) {
    const auto& param_node = param_list[arg.getArgNo()];

    // Create an alloca for this variable.
    auto const alloca = createEntryAlloca(func,
                                          arg.getName().str(),
                                          param_node.type->getType(ctx));

    // Store the initial value into the alloca.
    ctx.builder.CreateStore(&arg, alloca);

    const auto is_mutable
      = param_node.qualifier
        && (*param_node.qualifier == VariableQual::mutable_);

    // Add arguments to variable symbol table.
    argument_table.registOrOverwrite(
      arg.getName().str(),
      Variable{
        {alloca, param_node.type->createSignKindStack(ctx)},
        is_mutable
    });
  }

  return argument_table;
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CGContext&                         ctx,
                  llvm::legacy::FunctionPassManager& fp_manager) noexcept
    : ctx{ctx}
    , fp_manager{fp_manager}
  {
  }

  llvm::Function* operator()(ast::Nil) const
  {
    unreachable();
  }

  llvm::Function* operator()(const ast::FunctionDecl& node) const
  {
    const auto& params = *node.params;

    if (params.size() && params.at(0).is_variadic_args) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "requires a named argument before '...'")};
    }

    const auto is_variadic_args = isVariadicArgs(params);
    if (!is_variadic_args) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "cannot have multiple variable arguments")};
    }

    assert(!(*is_variadic_args && node.params.length() == 0));
    const auto named_params_length
      = *is_variadic_args ? node.params.length() - 1 : node.params.length();

    std::vector<llvm::Type*> param_types(named_params_length);

    for (std::size_t i = 0; i != named_params_length; ++i) {
      const auto& param_type = node.params[i].type;
      param_types.at(i)      = param_type->getType(ctx);
    }

    auto const func_type
      = llvm::FunctionType::get(node.return_type->getType(ctx),
                                param_types,
                                *is_variadic_args);

    const auto name = node.name.utf8();

    // Register return type to table.
    ctx.return_type_table.registOrOverwrite(name, node.return_type);

    auto const func
      = createLlvmFunction(node.linkage, func_type, name, *ctx.module);

    // Set names to all arguments.
    for (std::size_t idx = 0; auto&& arg : func->args())
      arg.setName(node.params[idx++].name.utf8());

    return func;
  }

  llvm::Function* operator()(const ast::FunctionDef& node) const
  {
    const auto name = node.decl.name.utf8();

    auto func = ctx.module->getFunction(name);

    if (func && !func->isDeclaration()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node.decl),
                        fmt::format("redefinition of '{}'", name))};
    }

    if (!func)
      func = this->operator()(node.decl);

    if (!func) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node.decl),
                        fmt::format("failed to create function '{}'", name))};
    }

    auto const entry_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    ctx.builder.SetInsertPoint(entry_bb);

    auto argument_table
      = createArgumentTable(ctx, func, node.decl.params, func->args());

    // Used to combine returns into one.
    auto const end_bb = llvm::BasicBlock::Create(ctx.context);
    // Return variable.
    auto const return_variable
      = node.decl.return_type->isVoid()
          ? nullptr
          : createEntryAlloca(func, "", node.decl.return_type->getType(ctx));

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
    std::vector<llvm::Type*> element_types;
    for (const auto& element : node.elements)
      element_types.emplace_back(element.type->getType(ctx));

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
          std::move(node.elements),
          llvm::StructType::create(ctx.context, element_types, name)));
    }

    return nullptr;
  }

private:
  CGContext& ctx;

  llvm::legacy::FunctionPassManager& fp_manager;
};

llvm::Function* createTopLevel(CGContext&                         ctx,
                               llvm::legacy::FunctionPassManager& fp_manager,
                               const ast::TopLevel&               node)
{
  return boost::apply_visitor(TopLevelVisitor{ctx, fp_manager}, node);
}

} // namespace maple::codegen
