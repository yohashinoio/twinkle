/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <emera/pch/pch.hpp>
#include <emera/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Identifier,
  (std::u32string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Path,
  (std::u32string, path)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::BuiltinType,
  (emera::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ArrayType,
  (emera::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::PointerType,
  (emera::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::UserDefinedType,
  (emera::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ReferenceType,
  (emera::ast::Type, refee_type)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::CharLiteral,
  (emera::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::BinOp,
  (emera::ast::Expr, lhs)
  (std::u32string, op)
  (emera::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::UnaryOp,
  (std::u32string, op)
  (emera::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Reference,
  (emera::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::New,
  (emera::ast::Type, type)
  (bool, with_init)
  (std::vector<emera::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Delete,
  (emera::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Dereference,
  (emera::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::MemberAccess,
  (emera::ast::Expr, lhs)
  (emera::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Subscript,
  (emera::ast::Expr, lhs)
  (emera::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::FunctionCall,
  (emera::ast::Expr, callee)
  (std::deque<emera::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Cast,
  (emera::ast::Expr, lhs)
  (emera::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Pipeline,
  (emera::ast::Expr, lhs)
  (std::u32string, op)
  (emera::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ArrayLiteral,
  (std::vector<emera::ast::Expr>, elements)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ClassLiteral,
  (emera::ast::Identifier, class_name)
  (std::vector<emera::ast::Expr>, initializer_list)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::BuiltinMacro,
  (emera::codegen::BuiltinMacroKind, kind)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::VariableDef,
  (std::optional<emera::VariableQual>, qualifier)
  (emera::ast::Identifier, name)
  (std::optional<emera::ast::Type>, type)
  (std::optional<emera::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Return,
  (std::optional<emera::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Assignment,
  (emera::ast::Expr, lhs)
  (std::u32string, op)
  (emera::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::PrefixIncrementDecrement,
  (std::u32string, op)
  (emera::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::If,
  (emera::ast::Expr, condition)
  (emera::ast::Stmt, then_statement)
  (std::optional<emera::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Loop,
  (emera::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::While,
  (emera::ast::Expr, cond_expr)
  (emera::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::For,
  (std::optional<emera::ast::ForInitVariant>, init_stmt)
  (std::optional<emera::ast::Expr>, cond_expr)
  (std::optional<emera::ast::ForLoopVariant>, loop_stmt)
  (emera::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ClassDecl,
  (emera::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::VariableDefWithoutInit,
  (std::optional<emera::VariableQual>, qualifier)
  (emera::ast::Identifier, name)
  (emera::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Constructor,
	(emera::ast::FunctionDecl, decl)
  (emera::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Destructor,
	(emera::ast::FunctionDecl, decl)
  (emera::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ClassDef,
  (bool, is_public)
  (emera::ast::Identifier, name)
  (emera::ast::ClassMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Parameter,
  (emera::ast::Identifier, name)
  (std::unordered_set<emera::VariableQual>, qualifier)
  (emera::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::ParameterList,
  (std::deque<emera::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::FunctionDecl,
  (emera::Linkage, linkage)
  (emera::ast::Identifier, name)
  (emera::ast::ParameterList, params)
  (emera::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::FunctionDef,
  (bool, is_public)
	(emera::ast::FunctionDecl, decl)
  (emera::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::Typedef,
  (emera::ast::Identifier, alias)
  (emera::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::RelativeImport,
  (emera::ast::Path, path)
)

BOOST_FUSION_ADAPT_STRUCT(
  emera::ast::TopLevelWithAttr,
  (emera::ast::Attrs, attrs)
  (emera::ast::TopLevel, top_level)
)

// clang-format on

#endif
