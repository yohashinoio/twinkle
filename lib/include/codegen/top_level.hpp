/**
 * top_level.hpp
 *
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

#include <pch/pch.hpp>
#include <codegen/codegen.hpp>
#include <codegen/common.hpp>

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CodeGenerator::Context&            ctx,
                  llvm::legacy::FunctionPassManager& fp_manager) noexcept;

  llvm::Function* operator()(ast::Nil) const;

  llvm::Function* operator()(const ast::FunctionDecl& node) const;

  llvm::Function* operator()(const ast::FunctionDef& node) const;

private:
  CodeGenerator::Context& ctx;

  llvm::legacy::FunctionPassManager& fp_manager;
};

} // namespace maple::codegen

#endif
