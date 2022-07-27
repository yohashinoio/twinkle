/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <spica/pch/pch.hpp>
#include <spica/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Identifier,
  (std::u32string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Path,
  (std::u32string, path)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::BuiltinType,
  (spica::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ArrayType,
  (spica::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::PointerType,
  (std::vector<boost::blank>, n_ops)
  (spica::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::UserDefinedType,
  (spica::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ReferenceType,
  (spica::ast::Type, refee_type)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::CharLiteral,
  (spica::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::BinOp,
  (spica::ast::Expr, lhs)
  (std::u32string, op)
  (spica::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::UnaryOp,
  (std::u32string, op)
  (spica::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Reference,
  (spica::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::New,
  (spica::ast::Type, type)
  (bool, with_init)
  (std::vector<spica::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Delete,
  (spica::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Dereference,
  (spica::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::MemberAccess,
  (spica::ast::Expr, lhs)
  (spica::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Subscript,
  (spica::ast::Expr, lhs)
  (spica::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::FunctionCall,
  (spica::ast::Expr, callee)
  (std::deque<spica::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Cast,
  (spica::ast::Expr, lhs)
  (spica::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Pipeline,
  (spica::ast::Expr, lhs)
  (std::u32string, op)
  (spica::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ArrayLiteral,
  (std::vector<spica::ast::Expr>, elements)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ClassLiteral,
  (spica::ast::Identifier, class_name)
  (std::vector<spica::ast::Expr>, initializer_list)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::BuiltinMacro,
  (spica::codegen::BuiltinMacroKind, kind)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::VariableDef,
  (std::optional<spica::VariableQual>, qualifier)
  (spica::ast::Identifier, name)
  (std::optional<spica::ast::Type>, type)
  (std::optional<spica::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Return,
  (std::optional<spica::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Assignment,
  (spica::ast::Expr, lhs)
  (std::u32string, op)
  (spica::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::PrefixIncrementDecrement,
  (std::u32string, op)
  (spica::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::If,
  (spica::ast::Expr, condition)
  (spica::ast::Stmt, then_statement)
  (std::optional<spica::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Loop,
  (spica::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::While,
  (spica::ast::Expr, cond_expr)
  (spica::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::For,
  (std::optional<spica::ast::ForInitVariant>, init_stmt)
  (std::optional<spica::ast::Expr>, cond_expr)
  (std::optional<spica::ast::ForLoopVariant>, loop_stmt)
  (spica::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ClassDecl,
  (spica::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::VariableDefWithoutInit,
  (std::optional<spica::VariableQual>, qualifier)
  (spica::ast::Identifier, name)
  (spica::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Constructor,
	(spica::ast::FunctionDecl, decl)
  (spica::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Destructor,
	(spica::ast::FunctionDecl, decl)
  (spica::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ClassDef,
  (bool, is_public)
  (spica::ast::Identifier, name)
  (spica::ast::ClassMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Parameter,
  (spica::ast::Identifier, name)
  (std::unordered_set<spica::VariableQual>, qualifier)
  (spica::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::ParameterList,
  (std::deque<spica::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::FunctionDecl,
  (spica::ast::Identifier, name)
  (spica::ast::ParameterList, params)
  (spica::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::FunctionDef,
  (bool, is_public)
	(spica::ast::FunctionDecl, decl)
  (spica::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::Typedef,
  (spica::ast::Identifier, alias)
  (spica::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::RelativeImport,
  (spica::ast::Path, path)
)

BOOST_FUSION_ADAPT_STRUCT(
  spica::ast::TopLevelWithAttr,
  (spica::ast::Attrs, attrs)
  (spica::ast::TopLevel, top_level)
)

// clang-format on

#endif
