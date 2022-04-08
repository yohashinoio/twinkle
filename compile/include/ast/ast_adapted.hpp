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
  miko::ast::StringLiteral,
  (std::string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::CharLiteral,
  (unsigned char, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::UnaryOp,
  (std::string, op)
  (miko::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::BinOp,
  (miko::ast::Expr, lhs)
  (std::string, op)
  (miko::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::VariableRef,
  (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::FunctionCall,
  (std::string, callee)
  (std::vector<miko::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Conversion,
  (miko::ast::Expr, lhs)
  (std::shared_ptr<miko::Type>, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::AddressOf,
  (miko::ast::Expr, lhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Indirection,
  (miko::ast::Expr, lhs)
)

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::VariableDef,
  (std::optional<miko::VariableQual>, qualifier)
  (std::string, name)
  (std::optional<std::shared_ptr<miko::Type>>, type)
  (std::optional<miko::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Return,
  (std::optional<miko::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Assignment,
  (miko::ast::Expr, lhs)
  (std::string, op)
  (miko::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Break,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Continue,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::If,
  (miko::ast::Expr, condition)
  (miko::ast::Stmt, then_statement)
  (std::optional<miko::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Loop,
  (std::string, tmp)
  (miko::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::While,
  (miko::ast::Expr, cond_expr)
  (miko::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::For,
  (std::optional<miko::ast::ForInit>, init_stmt)
  (std::optional<miko::ast::Expr>, cond_expr)
  (std::optional<miko::ast::Assignment>, loop_stmt)
  (miko::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::Parameter,
  (std::optional<miko::VariableQual>, qualifier)
  (std::string, name)
  (std::shared_ptr<miko::Type>, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::ParameterList,
  (std::vector<miko::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::FunctionDecl,
  (std::optional<miko::Linkage>, linkage)
  (std::string, name)
  (miko::ast::ParameterList, params)
  (std::shared_ptr<miko::Type>, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::FunctionDef,
	(miko::ast::FunctionDecl, decl)
  (miko::ast::Stmt, body)
)

// clang-format on

#endif
