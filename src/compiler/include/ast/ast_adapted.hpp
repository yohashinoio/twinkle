/**
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
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::CharLiteral,
  (custard::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Identifier,
  (std::u32string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::BinOp,
  (custard::ast::Expr, lhs)
  (std::u32string, op)
  (custard::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::UnaryOp,
  (std::u32string, op)
  (custard::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Subscript,
  (custard::ast::Identifier, ident)
  (custard::ast::Expr, nsubscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::FunctionCall,
  (custard::ast::Identifier, callee)
  (std::vector<custard::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Conversion,
  (custard::ast::Expr, lhs)
  (std::shared_ptr<custard::Type>, as)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::InitializerList,
  (std::vector<custard::ast::Expr>, inits)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::VariableDef,
  (std::optional<custard::VariableQual>, qualifier)
  (custard::ast::Identifier, name)
  (std::optional<std::shared_ptr<custard::Type>>, type)
  (std::optional<custard::ast::Initializer>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Return,
  (std::optional<custard::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Assignment,
  (custard::ast::Expr, lhs)
  (std::u32string, op)
  (custard::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::PrefixIncAndDec,
  (std::u32string, op)
  (custard::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Petrify,
  (custard::ast::Identifier, ident)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::If,
  (custard::ast::Expr, condition)
  (custard::ast::Stmt, then_statement)
  (std::optional<custard::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Loop,
  (custard::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::While,
  (custard::ast::Expr, cond_expr)
  (custard::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::For,
  (std::optional<custard::ast::ForInitVariant>, init_stmt)
  (std::optional<custard::ast::Expr>, cond_expr)
  (std::optional<custard::ast::ForLoopVariant>, loop_stmt)
  (custard::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::StructElement,
  (custard::ast::Identifier, name)
  (std::shared_ptr<custard::Type>, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::StructDecl,
  (custard::ast::Identifier, name)
  (std::shared_ptr<custard::Type>, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::Parameter,
  (custard::ast::Identifier, name)
  (std::optional<custard::VariableQual>, qualifier)
  (std::shared_ptr<custard::Type>, type)
  (bool, is_variadic_args)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::ParameterList,
  (std::vector<custard::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::FunctionDecl,
  (custard::Linkage, linkage)
  (custard::ast::Identifier, name)
  (custard::ast::ParameterList, params)
  (std::shared_ptr<custard::Type>, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  custard::ast::FunctionDef,
	(custard::ast::FunctionDecl, decl)
  (custard::ast::Stmt, body)
)

// clang-format on

#endif
