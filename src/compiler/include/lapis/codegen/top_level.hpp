/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _339045f6_bc82_11ec_8422_0242ac120002
#define _339045f6_bc82_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <lapis/pch/pch.hpp>
#include <lapis/codegen/codegen.hpp>
#include <lapis/codegen/common.hpp>

namespace lapis::codegen
{

// It is not mandatory to receive a return value.
llvm::Function* createTopLevel(CGContext&                         ctx,
                               llvm::legacy::FunctionPassManager& fpm,
                               const ast::TopLevelWithAttr&       node);

} // namespace lapis::codegen

#endif
