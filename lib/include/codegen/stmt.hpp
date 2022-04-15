/**
 * stmt.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _10b4dce2_bc80_11ec_8422_0242ac120002
#define _10b4dce2_bc80_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <codegen/codegen.hpp>
#include <codegen/common.hpp>

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

struct StmtVisitor : public boost::static_visitor<void> {
  StmtVisitor(CodeGenerator::Context& ctx,
              SymbolTable&            scope,
              llvm::AllocaInst*       retvar,
              llvm::BasicBlock*       end_bb,
              llvm::BasicBlock*       break_bb,
              llvm::BasicBlock*       continue_bb) noexcept;

  void operator()(ast::Nil) const
  {
    // Empty statement, so not processed.
  }

  void operator()(const ast::CompoundStmt& node) const
  {
    codegen_statement(node, scope, ctx, retvar, end_bb, break_bb, continue_bb);
  }

  void operator()(const ast::Expr& node) const;

  void operator()(const ast::Return& node) const;

  void operator()(const ast::VariableDef& node) const;

  void operator()(const ast::Assignment& node) const;

  void operator()(const ast::PrefixIncAndDec& node) const;

  void operator()(const ast::If& node) const;

  void operator()(const ast::Loop& node) const;

  void operator()(const ast::While& node) const;

  void operator()(const ast::For& node) const;

  void operator()(ast::Break) const
  {
    if (break_bb) // If in a loop.
      ctx.builder.CreateBr(break_bb);
  }

  void operator()(ast::Continue) const
  {
    if (continue_bb) // If in a loop.
      ctx.builder.CreateBr(continue_bb);
  }

  static void codegen_statement(const ast::Stmt&        statement,
                                const SymbolTable&      scope,
                                CodeGenerator::Context& ctx,
                                llvm::AllocaInst*       retvar,
                                llvm::BasicBlock*       end_bb,
                                llvm::BasicBlock*       break_bb,
                                llvm::BasicBlock*       continue_bb);

private:
  Value gen_assignable_value_from_expr(
    const ast::Expr&                                  node,
    const boost::iterator_range<maple::InputIterator> position) const;

  std::vector<llvm::Value*> gen_init_list(const ast::InitList& list) const;

  void init_array(llvm::AllocaInst*                array_alloca,
                  const std::vector<llvm::Value*>& init_list) const;

  llvm::AllocaInst*
  create_variable_with_type(const ast::Stmt&                node,
                            llvm::Function*                 func,
                            const std::string&              name,
                            const Type&                     type,
                            const std::optional<ast::Expr>& init) const;

  std::pair<llvm::AllocaInst*, bool /* Is signed */>
  create_variable_with_type_inference(
    const ast::Stmt&                node,
    llvm::Function*                 func,
    const std::string&              name,
    const std::optional<ast::Expr>& init) const;

  CodeGenerator::Context& ctx;

  SymbolTable& scope;

  // Used to combine returns into one.
  llvm::AllocaInst* retvar;
  llvm::BasicBlock* end_bb;

  // If not in loop, nullptr.
  llvm::BasicBlock* break_bb; // Basic block transitioned by break statement.
  llvm::BasicBlock*
    continue_bb; // Basic block transitioned by continue statement.
};

} // namespace maple::codegen

#endif
