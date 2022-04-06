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

using namespace miko::ast;
using namespace miko::id;

BOOST_FUSION_ADAPT_STRUCT(
  StringLiteral,
  (std::string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  CharLiteral,
  (unsigned char, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  UnaryOp,
  (std::string, op)
  (Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  BinOp,
  (Expr, lhs)
  (std::string, op)
  (Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  VariableRef,
  (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  FunctionCall,
  (std::string, callee)
  (std::vector<miko::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  Conversion,
  (Expr, lhs)
  (TypeInfo, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  AddressOf,
  (Expr, lhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  Indirection,
  (Expr, lhs)
)

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  VariableDef,
  (std::optional<VariableQualifier>, qualifier)
  (std::string, name)
  (TypeInfo, type)
  (std::optional<Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  Return,
  (std::optional<Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  Assignment,
  (Expr, lhs)
  (std::string, op)
  (Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  Break,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  Continue,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  If,
  (Expr, condition)
  (Stmt, then_statement)
  (std::optional<Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  Loop,
  (std::string, tmp)
  (Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  While,
  (Expr, cond_expr)
  (Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  For,
  (std::optional<ForInit>, init_stmt)
  (std::optional<Expr>, cond_expr)
  (std::optional<Assignment>, loop_stmt)
  (Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  Parameter,
  (std::optional<VariableQualifier>, qualifier)
  (std::string, name)
  (TypeInfo, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  ParameterList,
  (std::vector<Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  FunctionDecl,
  (std::optional<FunctionLinkage>, linkage)
  (std::string, name)
  (ParameterList, params)
  (TypeInfo, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  FunctionDef,
	(FunctionDecl, decl)
  (Stmt, body)
)

// clang-format on

#endif
