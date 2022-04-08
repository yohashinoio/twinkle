/**
 * ast_adapted.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <pch/pch.hpp>
#include <ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::StringLiteral,
  (std::string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::CharLiteral,
  (unsigned char, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::UnaryOp,
  (std::string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::BinOp,
  (maple::ast::Expr, lhs)
  (std::string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::VariableRef,
  (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::FunctionCall,
  (std::string, callee)
  (std::vector<maple::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Conversion,
  (maple::ast::Expr, lhs)
  (std::shared_ptr<maple::Type>, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::AddressOf,
  (maple::ast::Expr, lhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Indirection,
  (maple::ast::Expr, lhs)
)

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::VariableDef,
  (std::optional<maple::VariableQual>, qualifier)
  (std::string, name)
  (std::optional<std::shared_ptr<maple::Type>>, type)
  (std::optional<maple::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Return,
  (std::optional<maple::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Assignment,
  (maple::ast::Expr, lhs)
  (std::string, op)
  (maple::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Break,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Continue,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::If,
  (maple::ast::Expr, condition)
  (maple::ast::Stmt, then_statement)
  (std::optional<maple::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Loop,
  (std::string, tmp)
  (maple::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::While,
  (maple::ast::Expr, cond_expr)
  (maple::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::For,
  (std::optional<maple::ast::ForInit>, init_stmt)
  (std::optional<maple::ast::Expr>, cond_expr)
  (std::optional<maple::ast::Assignment>, loop_stmt)
  (maple::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::Parameter,
  (std::optional<maple::VariableQual>, qualifier)
  (std::string, name)
  (std::shared_ptr<maple::Type>, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::ParameterList,
  (std::vector<maple::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::FunctionDecl,
  (std::optional<maple::Linkage>, linkage)
  (std::string, name)
  (maple::ast::ParameterList, params)
  (std::shared_ptr<maple::Type>, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  maple::ast::FunctionDef,
	(maple::ast::FunctionDecl, decl)
  (maple::ast::Stmt, body)
)

// clang-format on

#endif
