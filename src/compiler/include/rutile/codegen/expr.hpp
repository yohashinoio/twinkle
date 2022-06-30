/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _3b506594_bc7f_11ec_8422_0242ac120002
#define _3b506594_bc7f_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <rutile/pch/pch.hpp>
#include <rutile/codegen/codegen.hpp>
#include <rutile/codegen/common.hpp>

namespace rutile::codegen
{

[[nodiscard]] llvm::Function*
findFunction(CGContext& ctx,
             const std::vector<std::string>&
               mangled_names /* Assuming they are in order of priority */);

[[nodiscard]] Value createExpr(CGContext&         ctx,
                               const SymbolTable& scope,
                               const StmtContext& stmt_ctx,
                               const ast::Expr&   expr);

} // namespace rutile::codegen

#endif
