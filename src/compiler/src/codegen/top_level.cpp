/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/top_level.hpp>
#include <codegen/stmt.hpp>
#include <codegen/exception.hpp>

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
    auto const alloca
      = createEntryAlloca(func,
                          arg.getName().str(),
                          param_node.type->getType(ctx.context));

    // Store the initial value into the alloca.
    ctx.builder.CreateStore(&arg, alloca);

    const auto is_mutable
      = param_node.qualifier
        && (*param_node.qualifier == VariableQual::mutable_);

    // Add arguments to variable symbol table.
    argument_table.regist(arg.getName().str(),
                          {
                            {alloca, param_node.type->createSignKindStack()},
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
      param_types.at(i)      = param_type->getType(ctx.context);
    }

    auto const func_type
      = llvm::FunctionType::get(node.return_type->getType(ctx.context),
                                param_types,
                                *is_variadic_args);

    const auto name = node.name.utf8();

    // Register return type to table.
    ctx.func_ret_types.regist(name, node.return_type);

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
    if (func) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node.decl),
                        fmt::format("redefinition of '{}'", name))};
    }

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
          : createEntryAlloca(func,
                              "",
                              node.decl.return_type->getType(ctx.context));

    createStatement(ctx,
                    argument_table,
                    node.body,
                    return_variable,
                    end_bb,
                    nullptr,
                    nullptr);

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

    verifyFunction(func, ctx.positions.position_of(node));

    fp_manager.run(*func);

    return func;
  }

  llvm::Function* operator()(const ast::StructDecl& node) const
  {
    // TODO
    return nullptr;
  }

private:
  // Throws an exception if verification fails.
  void verifyFunction(llvm::Function* const                       func,
                      const boost::iterator_range<InputIterator>& pos) const
  {
    std::string              err_string;
    llvm::raw_string_ostream err_stream{err_string};

    if (llvm::verifyFunction(*func, &err_stream)) {
      func->eraseFromParent();

      throw CodegenError{ctx.formatError(pos, err_stream.str())};
    }
  }

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
