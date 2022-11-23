/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _3b506594_bc7f_11ec_8422_0242ac120002
#define _3b506594_bc7f_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>
#include <twinkle/codegen/codegen.hpp>
#include <twinkle/codegen/common.hpp>

namespace twinkle::codegen
{

[[nodiscard]] llvm::Value* gepByOffset(CGContext&          ctx,
                                       llvm::Value* const  value,
                                       llvm::Type* const   type,
                                       const std::uint32_t offset);

[[nodiscard]] llvm::Function*
findFunction(CGContext& ctx,
             const std::vector<std::string>&
               mangled_names /* Assuming they are in order of priority */);

struct ScopeResolutionResult {
  ScopeResolutionResult(std::vector<ast::Expr>&& ns_names,
                        const ast::Expr&         expr)
    : ns_names{std::move(ns_names)}
    , expr{expr}
  {
  }

  // A::B::C()
  // ^~~~
  std::vector<ast::Expr> ns_names;

  // A::B::C()
  //       ^~~
  const ast::Expr& expr;
};

[[nodiscard]] ScopeResolutionResult
createScopeResolutionResult(CGContext& ctx, const ast::ScopeResolution& node);

[[nodiscard]] Value createExpr(CGContext&         ctx,
                               const SymbolTable& scope,
                               const StmtContext& stmt_ctx,
                               const ast::Expr&   expr);

} // namespace twinkle::codegen

#endif
