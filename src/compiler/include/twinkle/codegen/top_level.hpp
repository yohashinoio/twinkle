/**
 * These codes are licensed under LGPL-2.1 License
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

// Indicates whether methods will be declared, defined, or both
enum class MethodGeneration {
  define_and_declare,
  declare,
  define,
};

void createClass(CGContext&                        ctx,
                 const ast::ClassDef&              node,
                 const MethodGeneration            method_conv,
                 // Use this when you want to include the type in the class
                 // name, for example, in a template class
                 const std::optional<std::string>& custom_class_name
                 = std::nullopt);

using ClassMethods = std::vector<ast::FunctionDef>;

void defineMethods(CGContext&          ctx,
                   const ClassMethods& methods,
                   const std::string&  class_name);

void declareMethods(CGContext&          ctx,
                    const ClassMethods& methods,
                    const std::string&  class_name);

llvm::Function* createTopLevel(CGContext& ctx, const ast::TopLevel& node);

llvm::Function* createTopLevel(CGContext&                   ctx,
                               const ast::TopLevelWithAttr& node);

} // namespace twinkle::codegen

#endif
