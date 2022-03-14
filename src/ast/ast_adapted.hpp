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

namespace ast = miko::ast;

// clang-format off

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  ast::unary_op_expr,
  (std::string, op)
  (ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::binary_op_expr,
  (ast::expression, lhs)
  (std::string, op)
  (ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::variable_expr,
  (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::function_call_expr,
  (std::string, callee)
  (std::vector<ast::expression>, args)
)

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  ast::variable_def_statement,
  (std::optional<miko::id::variable_qualifier>, qualifier)
  (std::string, name)
  (std::optional<ast::expression>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::return_statement,
  (ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::if_statement,
  (ast::expression, condition)
  (ast::compound_statement, then_statement)
  (std::optional<ast::compound_statement>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::for_statement,
  (std::optional<ast::expression>, init_expression)
  (std::optional<ast::expression>, cond_expression)
  (std::optional<ast::expression>, loop_expression)
  (ast::compound_statement, body)
)

//===----------------------------------------------------------------------===//
// Top level statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  ast::function_declare,
  (std::optional<miko::id::function_linkage>, linkage)
  (std::string, name)
  (std::vector<std::string>, args)
  (miko::id::data_type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::function_define,
	(ast::function_declare, decl)
  (ast::compound_statement, body)
)

// clang-format on

#endif
