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

#include <spica/pch/pch.hpp>
#include <spica/codegen/codegen.hpp>
#include <spica/codegen/common.hpp>

namespace spica::codegen
{

[[nodiscard]] std::optional<bool>
isVariadicArgs(const ast::ParameterList& params);

[[nodiscard]] std::vector<llvm::Type*>
createParamTypes(CGContext&                ctx,
                 const ast::ParameterList& params,
                 const std::size_t         named_params_len);

void createFunctionBody(CGContext&                  ctx,
                        llvm::Function* const       func,
                        const std::string_view      name,
                        const ast::ParameterList&   params,
                        const std::shared_ptr<Type> return_type,
                        const ast::Stmt&            body);

llvm::Function* createTopLevel(CGContext& ctx, const ast::TopLevel& node);

llvm::Function* createTopLevel(CGContext&                   ctx,
                               const ast::TopLevelWithAttr& node);

} // namespace spica::codegen

#endif
