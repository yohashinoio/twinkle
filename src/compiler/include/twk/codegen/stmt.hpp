/**
 * These codes are licensed under MIT License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _10b4dce2_bc80_11ec_8422_0242ac120002
#define _10b4dce2_bc80_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twk/pch/pch.hpp>
#include <twk/codegen/codegen.hpp>
#include <twk/codegen/common.hpp>

namespace twk::codegen
{

void invokeDestructor(CGContext& ctx, const Value& this_);

void createStatement(CGContext&         ctx,
                     const SymbolTable& scope_arg,
                     const StmtContext& stmt_ctx_arg,
                     const ast::Stmt&   statement);

} // namespace twk::codegen

#endif
