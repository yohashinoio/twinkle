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

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CodeGenerator::Context& ctx, SymbolTable& scope) noexcept;

  [[nodiscard]] Value operator()(ast::Nil) const
  {
    unreachable();
  }

  // 32bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint32_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node), false};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt32Ty(), node), true};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), node), false};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt64Ty(), node), true};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {
      ctx.int1ToBool(llvm::ConstantInt::get(ctx.builder.getInt1Ty(), node)),
      false};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return Value{ctx.builder.CreateGlobalStringPtr(node.str, ".str")};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Char literal is u8.
    return {llvm::ConstantInt::get(ctx.builder.getInt8Ty(), node.ch), false};
  }

  [[nodiscard]] Value operator()(const ast::Identifier& node) const;

  [[nodiscard]] Value operator()(const ast::BinOp& node) const;

  [[nodiscard]] Value operator()(const ast::UnaryOp& node) const;

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const;

  [[nodiscard]] Value operator()(const ast::Conversion& node) const;

private:
  [[nodiscard]] Value gen_address_of(const Value& rhs) const;

  [[nodiscard]] Value
  gen_indirection(const boost::iterator_range<maple::InputIterator> pos,
                  const Value&                                      rhs) const;

  CodeGenerator::Context& ctx;

  SymbolTable& scope;
};

} // namespace maple::codegen

#endif
