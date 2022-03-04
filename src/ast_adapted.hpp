//
//  ast_adapted.hpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include "pch.hpp"
#include "ast.hpp"

namespace ast = miko::ast;

// clang-format off
BOOST_FUSION_ADAPT_STRUCT(
  ast::unaryop,
  (std::string, op)
  (ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::binop,
  (ast::expression, lhs)
  (std::string, op)
  (ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::return_statement,
  (ast::expression, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::function_decl,
  (std::string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  ast::function_def,
	(ast::function_decl, decl)
  (ast::compound_statement, body)
)
// clang-format on

#endif
