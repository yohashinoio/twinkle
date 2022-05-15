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

#include <custard/pch/pch.hpp>
#include <custard/codegen/codegen.hpp>
#include <custard/codegen/common.hpp>

namespace custard::codegen
{

// It is not mandatory to receive a return value.
llvm::Function* createTopLevel(CGContext&                         ctx,
                               llvm::legacy::FunctionPassManager& fp_manager,
                               const ast::TopLevel&               node);

} // namespace custard::codegen

#endif
