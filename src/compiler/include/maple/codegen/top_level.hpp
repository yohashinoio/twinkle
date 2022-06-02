/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _339045f6_bc82_11ec_8422_0242ac120002
#define _339045f6_bc82_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <maple/pch/pch.hpp>
#include <maple/codegen/codegen.hpp>
#include <maple/codegen/common.hpp>

namespace maple::codegen
{

// It is not mandatory to receive a return value.
llvm::Function* createTopLevel(CGContext&                         ctx,
                               llvm::legacy::FunctionPassManager& fp_manager,
                               const ast::TopLevelWithAttr&       node);

} // namespace maple::codegen

#endif
