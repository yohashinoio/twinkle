/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <maple/pch/pch.hpp>
#include <maple/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Identifier,
  (std::u32string, name)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::BuiltinType,
  (maple::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::ArrayType,
  (maple::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::PointerType,
  (maple::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::UserDefinedType,
  (maple::ast::Identifier, name)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::CharLiteral,
  (maple::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::BinOp,
  (maple::ast::Expr, lhs)
  (std::u32string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::UnaryOp,
  (std::u32string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::MemberAccess,
  (maple::ast::Expr, lhs)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Subscript,
  (maple::ast::Expr, lhs)
  (maple::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::FunctionCall,
  (maple::ast::Expr, callee)
  (std::deque<maple::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Conversion,
  (maple::ast::Expr, lhs)
  (maple::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Pipeline,
  (maple::ast::Expr, lhs)
  (std::u32string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::UniformInit,
  (maple::ast::Identifier, ident)
  (std::vector<maple::ast::Expr>, initializer_list)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::InitializerList,
  (std::vector<maple::ast::Expr>, inits)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::VariableDef,
  (std::optional<maple::VariableQual>, qualifier)
  (maple::ast::Identifier, name)
  (std::optional<maple::ast::Type>, type)
  (std::optional<maple::ast::Initializer>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Return,
  (std::optional<maple::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Assignment,
  (maple::ast::Expr, lhs)
  (std::u32string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::PrefixIncAndDec,
  (std::u32string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::If,
  (maple::ast::Expr, condition)
  (maple::ast::Stmt, then_statement)
  (std::optional<maple::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Loop,
  (maple::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::While,
  (maple::ast::Expr, cond_expr)
  (maple::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::For,
  (std::optional<maple::ast::ForInitVariant>, init_stmt)
  (std::optional<maple::ast::Expr>, cond_expr)
  (std::optional<maple::ast::ForLoopVariant>, loop_stmt)
  (maple::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::StructDecl,
  (maple::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::VariableDefWithoutInit,
  (maple::ast::Identifier, name)
  (maple::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::StructDef,
  (maple::ast::Identifier, name)
  (maple::ast::StructMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Parameter,
  (maple::ast::Identifier, name)
  (std::optional<maple::VariableQual>, qualifier)
  (maple::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::ParameterList,
  (std::deque<maple::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::FunctionDecl,
  (maple::Linkage, linkage)
  (maple::ast::Identifier, name)
  (maple::ast::ParameterList, params)
  (maple::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::FunctionDef,
	(maple::ast::FunctionDecl, decl)
  (maple::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::TopLevelWithAttr,
  (maple::ast::Attrs, attrs)
  (maple::ast::TopLevel, top_level)
)

// clang-format on

#endif
