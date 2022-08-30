/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _339045f6_bc82_11ec_8422_0242ac120002
#define _339045f6_bc82_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>
#include <twinkle/codegen/codegen.hpp>
#include <twinkle/codegen/common.hpp>

namespace twinkle::codegen
{

[[nodiscard]] std::optional<bool>
isVariadicArgs(const ast::ParameterList& params);

void createFunctionBody(CGContext&                  ctx,
                        llvm::Function* const       func,
                        const std::string_view      name,
                        const ast::ParameterList&   params,
                        const std::shared_ptr<Type> return_type,
                        const ast::Stmt&            body);

[[nodiscard]] llvm::Function*
declareFunction(CGContext&                   ctx,
                const ast::FunctionDecl&     node,
                const std::string_view       mangled_name,
                const std::shared_ptr<Type>& return_type);

[[nodiscard]] std::vector<twinkle::ast::FunctionDef>
createClassNoMethodDeclDef(CGContext& ctx, const ast::ClassDef& node);

void defineMethods(CGContext&                           ctx,
                   const std::vector<ast::FunctionDef>& methods,
                   const std::string&                   class_name);

void declareMethods(CGContext&                           ctx,
                    const std::vector<ast::FunctionDef>& methods,
                    const std::string&                   class_name);

llvm::Function* createTopLevel(CGContext& ctx, const ast::TopLevel& node);

llvm::Function* createTopLevel(CGContext&                   ctx,
                               const ast::TopLevelWithAttr& node);

} // namespace twinkle::codegen

#endif
