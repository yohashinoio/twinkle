/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <lapis/pch/pch.hpp>
#include <lapis/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Identifier,
  (std::u32string, name)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::BuiltinType,
  (lapis::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::ArrayType,
  (lapis::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::PointerType,
  (lapis::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::UserDefinedType,
  (lapis::ast::Identifier, name)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::CharLiteral,
  (lapis::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::BinOp,
  (lapis::ast::Expr, lhs)
  (std::u32string, op)
  (lapis::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::UnaryOp,
  (std::u32string, op)
  (lapis::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::MemberAccess,
  (lapis::ast::Expr, lhs)
  (lapis::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Subscript,
  (lapis::ast::Expr, lhs)
  (lapis::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::FunctionCall,
  (lapis::ast::Expr, callee)
  (std::deque<lapis::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Cast,
  (lapis::ast::Expr, lhs)
  (lapis::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Pipeline,
  (lapis::ast::Expr, lhs)
  (std::u32string, op)
  (lapis::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::UniformInit,
  (lapis::ast::Identifier, class_name)
  (std::vector<lapis::ast::Expr>, initializer_list)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::InitializerList,
  (std::vector<lapis::ast::Expr>, inits)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::VariableDef,
  (std::optional<lapis::VariableQual>, qualifier)
  (lapis::ast::Identifier, name)
  (std::optional<lapis::ast::Type>, type)
  (std::optional<lapis::ast::Initializer>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Return,
  (std::optional<lapis::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Assignment,
  (lapis::ast::Expr, lhs)
  (std::u32string, op)
  (lapis::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::PrefixIncAndDec,
  (std::u32string, op)
  (lapis::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::If,
  (lapis::ast::Expr, condition)
  (lapis::ast::Stmt, then_statement)
  (std::optional<lapis::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Loop,
  (lapis::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::While,
  (lapis::ast::Expr, cond_expr)
  (lapis::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::For,
  (std::optional<lapis::ast::ForInitVariant>, init_stmt)
  (std::optional<lapis::ast::Expr>, cond_expr)
  (std::optional<lapis::ast::ForLoopVariant>, loop_stmt)
  (lapis::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::ClassDecl,
  (lapis::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::VariableDefWithoutInit,
  (lapis::ast::Identifier, name)
  (lapis::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Constructor,
	(lapis::ast::FunctionDecl, decl)
  (lapis::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Destructor,
	(lapis::ast::FunctionDecl, decl)
  (lapis::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::ClassDef,
  (lapis::ast::Identifier, name)
  (lapis::ast::ClassMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Parameter,
  (lapis::ast::Identifier, name)
  (std::optional<lapis::VariableQual>, qualifier)
  (lapis::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::ParameterList,
  (std::deque<lapis::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::FunctionDecl,
  (lapis::Linkage, linkage)
  (lapis::ast::Identifier, name)
  (lapis::ast::ParameterList, params)
  (lapis::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::FunctionDef,
	(lapis::ast::FunctionDecl, decl)
  (lapis::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::Typedef,
  (lapis::ast::Identifier, alias)
  (lapis::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  lapis::ast::TopLevelWithAttr,
  (lapis::ast::Attrs, attrs)
  (lapis::ast::TopLevel, top_level)
)

// clang-format on

#endif
