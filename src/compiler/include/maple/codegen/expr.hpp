/**
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

#include <maple/pch/pch.hpp>
#include <maple/codegen/codegen.hpp>
#include <maple/codegen/common.hpp>

namespace maple::codegen
{

// Be careful about the lifetime of the return value references.
// Lifetime depends on the argument 3!
[[nodiscard]] Variable&
findVariable(CGContext& ctx, const ast::Identifier& node, SymbolTable& scope);

[[nodiscard]] Value
createExpr(CGContext& ctx, SymbolTable& scope, const ast::Expr& expr);

} // namespace maple::codegen

#endif
