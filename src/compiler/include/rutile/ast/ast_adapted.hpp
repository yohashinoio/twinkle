/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <rutile/pch/pch.hpp>
#include <rutile/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Identifier,
  (std::u32string, name)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::BuiltinType,
  (rutile::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::ArrayType,
  (rutile::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::PointerType,
  (rutile::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::UserDefinedType,
  (rutile::ast::Identifier, name)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::CharLiteral,
  (rutile::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::BinOp,
  (rutile::ast::Expr, lhs)
  (std::u32string, op)
  (rutile::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::UnaryOp,
  (std::u32string, op)
  (rutile::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Dereference,
  (rutile::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::MemberAccess,
  (rutile::ast::Expr, lhs)
  (rutile::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Subscript,
  (rutile::ast::Expr, lhs)
  (rutile::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::FunctionCall,
  (rutile::ast::Expr, callee)
  (std::deque<rutile::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Cast,
  (rutile::ast::Expr, lhs)
  (rutile::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Pipeline,
  (rutile::ast::Expr, lhs)
  (std::u32string, op)
  (rutile::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::ArrayLiteral,
  (std::vector<rutile::ast::Expr>, elements)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::ClassLiteral,
  (rutile::ast::Identifier, class_name)
  (std::vector<rutile::ast::Expr>, initializer_list)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::VariableDef,
  (std::optional<rutile::VariableQual>, qualifier)
  (rutile::ast::Identifier, name)
  (std::optional<rutile::ast::Type>, type)
  (std::optional<rutile::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Return,
  (std::optional<rutile::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Assignment,
  (rutile::ast::Expr, lhs)
  (std::u32string, op)
  (rutile::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::PrefixIncAndDec,
  (std::u32string, op)
  (rutile::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::If,
  (rutile::ast::Expr, condition)
  (rutile::ast::Stmt, then_statement)
  (std::optional<rutile::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Loop,
  (rutile::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::While,
  (rutile::ast::Expr, cond_expr)
  (rutile::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::For,
  (std::optional<rutile::ast::ForInitVariant>, init_stmt)
  (std::optional<rutile::ast::Expr>, cond_expr)
  (std::optional<rutile::ast::ForLoopVariant>, loop_stmt)
  (rutile::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::ClassDecl,
  (rutile::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::VariableDefWithoutInit,
  (rutile::ast::Identifier, name)
  (rutile::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Constructor,
	(rutile::ast::FunctionDecl, decl)
  (rutile::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Destructor,
	(rutile::ast::FunctionDecl, decl)
  (rutile::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::ClassDef,
  (rutile::ast::Identifier, name)
  (rutile::ast::ClassMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Parameter,
  (rutile::ast::Identifier, name)
  (std::optional<rutile::VariableQual>, qualifier)
  (rutile::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::ParameterList,
  (std::deque<rutile::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::FunctionDecl,
  (rutile::Linkage, linkage)
  (rutile::ast::Identifier, name)
  (rutile::ast::ParameterList, params)
  (rutile::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::FunctionDef,
	(rutile::ast::FunctionDecl, decl)
  (rutile::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::Typedef,
  (rutile::ast::Identifier, alias)
  (rutile::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  rutile::ast::TopLevelWithAttr,
  (rutile::ast::Attrs, attrs)
  (rutile::ast::TopLevel, top_level)
)

// clang-format on

#endif
