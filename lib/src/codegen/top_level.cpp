/**
 * top_level.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/top_level.hpp>
#include <codegen/stmt.hpp>
#include <utils/format.hpp>

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CGContext&                         ctx,
                  llvm::legacy::FunctionPassManager& fp_manager) noexcept;

  llvm::Function* operator()(ast::Nil) const;

  llvm::Function* operator()(const ast::FunctionDecl& node) const;

  llvm::Function* operator()(const ast::FunctionDef& node) const;

private:
  CGContext& ctx;

  llvm::legacy::FunctionPassManager& fp_manager;
};

TopLevelVisitor::TopLevelVisitor(
  CGContext&                         ctx,
  llvm::legacy::FunctionPassManager& fp_manager) noexcept
  : ctx{ctx}
  , fp_manager{fp_manager}
{
}

llvm::Function* TopLevelVisitor::operator()(ast::Nil) const
{
  throw std::runtime_error{format_error_message(
    ctx.file.string(),
    "a function was executed that did not predict execution")};
}

llvm::Function* TopLevelVisitor::operator()(const ast::FunctionDecl& node) const
{
  auto&& ps = *node.params;

  if (ps.size() && ps.at(0).is_vararg) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "requires a named argument before '...'")};
  }

  bool is_vararg = false;
  for (const auto& r : ps) {
    if (r.is_vararg) {
      if (is_vararg) {
        throw std::runtime_error{
          ctx.formatError(ctx.positions.position_of(node),
                          "cannot have multiple variable arguments")};
      }
      else
        is_vararg = true;
    }
  }

  const auto named_params_length
    = is_vararg ? node.params.length() - 1 : node.params.length();
  std::vector<llvm::Type*> param_types(named_params_length);

  for (std::size_t i = 0; i != named_params_length; ++i) {
    const auto& param_type = node.params[i].type;
    param_types.at(i)      = param_type->getType(ctx.context);
  }

  auto const func_type
    = llvm::FunctionType::get(node.return_type->getType(ctx.context),
                              param_types,
                              is_vararg);

  llvm::Function* func;
  if (!node.linkage) {
    // External linkage.
    func = llvm::Function::Create(func_type,
                                  llvm::Function::ExternalLinkage,
                                  node.name,
                                  *ctx.module);
  }
  else if (node.linkage == Linkage::internal) {
    // Internal linkage.
    func = llvm::Function::Create(func_type,
                                  llvm::Function::InternalLinkage,
                                  node.name,
                                  *ctx.module);
  }

  // Set names for all arguments.
  for (std::size_t idx = 0; auto&& arg : func->args())
    arg.setName(node.params[idx++].name);

  return func;
}

llvm::Function* TopLevelVisitor::operator()(const ast::FunctionDef& node) const
{
  auto func = ctx.module->getFunction(node.decl.name);

  if (!func)
    func = this->operator()(node.decl);

  if (!func) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("failed to create function %s", node.decl.name))};
  }

  SymbolTable argument_table;

  auto const entry_bb = llvm::BasicBlock::Create(ctx.context, "", func);
  ctx.builder.SetInsertPoint(entry_bb);

  for (auto&& arg : func->args()) {
    const auto& param_node = node.decl.params[arg.getArgNo()];

    // Create an alloca for this variable.
    auto const alloca
      = createEntryAlloca(func,
                          arg.getName().str(),
                          param_node.type->getType(ctx.context));

    // Store the initial value into the alloca.
    ctx.builder.CreateStore(&arg, alloca);

    // Add arguments to variable symbol table.
    if (!param_node.qualifier) {
      // constant variable.
      argument_table.regist(arg.getName().str(),
                            {alloca, false, param_node.type->isSigned()});
    }
    else if (*param_node.qualifier == VariableQual::mutable_) {
      // mutable variable.
      argument_table.regist(arg.getName().str(),
                            {alloca, true, param_node.type->isSigned()});
    }
  }

  // Used to combine returns into one.
  auto const end_bb = llvm::BasicBlock::Create(ctx.context);
  auto const retvar
    = node.decl.return_type->isVoid()
        ? nullptr
        : createEntryAlloca(func,
                            "",
                            node.decl.return_type->getType(ctx.context));

  genStmt(ctx, argument_table, node.body, retvar, end_bb, nullptr, nullptr);

  // If there is no return, returns undef.
  if (!ctx.builder.GetInsertBlock()->getTerminator()
      && !(node.decl.return_type->isVoid())) {
    // Return 0 specially for main.
    if (node.decl.name == "main") {
      ctx.builder.CreateStore(
        llvm::ConstantInt::getSigned(func->getReturnType(), 0),
        retvar);
      ctx.builder.CreateBr(end_bb);
    }
    else {
      ctx.builder.CreateStore(llvm::UndefValue::get(func->getReturnType()),
                              retvar);
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

  if (retvar) {
    auto const retval
      = ctx.builder.CreateLoad(retvar->getAllocatedType(), retvar);
    ctx.builder.CreateRet(retval);
  }
  else {
    // Function that returns void.
    ctx.builder.CreateRet(nullptr);
  }

  std::string              em;
  llvm::raw_string_ostream os{em};
  if (llvm::verifyFunction(*func, &os)) {
    func->eraseFromParent();

    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node), os.str())};
  }

  fp_manager.run(*func);

  return func;
}

llvm::Function* genTopLevel(CGContext&                         ctx,
                            llvm::legacy::FunctionPassManager& fp_manager,
                            const ast::TopLevel&               node)
{
  return boost::apply_visitor(TopLevelVisitor{ctx, fp_manager}, node);
}

} // namespace maple::codegen
