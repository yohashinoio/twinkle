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

void createStatement(CGContext&        ctx,
                     SymbolTable&      scope,
                     const ast::Stmt&  statement,
                     llvm::AllocaInst* retvar,
                     llvm::BasicBlock* end_bb,
                     llvm::BasicBlock* break_bb,
                     llvm::BasicBlock* continue_bb);

} // namespace maple::codegen

#endif
