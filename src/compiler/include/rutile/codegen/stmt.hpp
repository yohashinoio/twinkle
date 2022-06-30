/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _10b4dce2_bc80_11ec_8422_0242ac120002
#define _10b4dce2_bc80_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <rutile/pch/pch.hpp>
#include <rutile/codegen/codegen.hpp>
#include <rutile/codegen/common.hpp>

namespace rutile::codegen
{

void createStatement(CGContext&         ctx,
                     const SymbolTable& scope_arg,
                     const StmtContext& stmt_ctx_arg,
                     const ast::Stmt&   statement);

} // namespace rutile::codegen

#endif
