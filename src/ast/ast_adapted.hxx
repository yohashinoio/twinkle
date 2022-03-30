/**
 * ast_adapted.hxx
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <pch/pch.hxx>
#include <ast/ast.hxx>

// clang-format off

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree adapt
//===----------------------------------------------------------------------===//


BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::string_literal,
  (std::string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::unary_op_expr,
  (std::string, op)
  (miko::ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::binary_op_expr,
  (miko::ast::expression, lhs)
  (std::string, op)
  (miko::ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::variable_expr,
  (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::function_call_expr,
  (std::string, callee)
  (std::vector<miko::ast::expression>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::cast_expr,
  (miko::ast::expression, lhs)
  (miko::ast::type_info, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::address_of_expr,
  (miko::ast::expression, lhs)
)

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::variable_def_statement,
  (std::optional<miko::id::variable_qualifier>, qualifier)
  (std::string, name)
  (miko::ast::type_info, type)
  (std::optional<miko::ast::expression>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::return_statement,
  (std::optional<miko::ast::expression>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::break_statement,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::continue_statement,
  (std::string, tmp)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::if_statement,
  (miko::ast::expression, condition)
  (miko::ast::statement, then_statement)
  (std::optional<miko::ast::statement>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::for_statement,
  (std::optional<miko::ast::expression>, init_expression)
  (std::optional<miko::ast::expression>, cond_expression)
  (std::optional<miko::ast::expression>, loop_expression)
  (miko::ast::statement, body)
)

//===----------------------------------------------------------------------===//
// Top level statement abstract syntax tree adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::parameter,
  (std::optional<miko::id::variable_qualifier>, qualifier)
  (std::string, name)
  (miko::ast::type_info, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::function_declare,
  (std::optional<miko::id::function_linkage>, linkage)
  (std::string, name)
  (std::vector<miko::ast::parameter>, params)
  (miko::ast::type_info, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  miko::ast::function_define,
	(miko::ast::function_declare, decl)
  (miko::ast::statement, body)
)

// clang-format on

#endif
