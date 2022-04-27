/**
 * expr.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _3b506594_bc7f_11ec_8422_0242ac120002
#define _3b506594_bc7f_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <codegen/codegen.hpp>
#include <codegen/common.hpp>

namespace maple::codegen
{

// Throws exception if not found.
[[nodiscard]] Variable findVariable(CGContext&             ctx,
                                    const ast::Identifier& node,
                                    const SymbolTable&     scope);

[[nodiscard]] Value
createExpr(CGContext& ctx, SymbolTable& scope, const ast::Expr& expr);

} // namespace maple::codegen

#endif
