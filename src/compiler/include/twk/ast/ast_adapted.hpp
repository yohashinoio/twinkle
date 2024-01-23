/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <twk/pch/pch.hpp>
#include <twk/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Identifier,
  (std::u32string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Path,
  (std::u32string, path)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::TemplateParameters,
  (twk::ast::TemplateParameters::TypeNames, type_names)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::TemplateArguments,
  (twk::ast::TemplateArguments::Types, types)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::BuiltinType,
  (twk::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ArrayType,
  (twk::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::PointerType,
  (std::vector<boost::blank>, n_ops)
  (twk::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::UserDefinedType,
  (twk::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::UserDefinedTemplateType,
  (twk::ast::UserDefinedType, template_type)
  (twk::ast::TemplateArguments, template_args)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ReferenceType,
  (twk::ast::Type, refee_type)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Value,
  (std::shared_ptr<twk::codegen::Value>, value)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::NullPointer,
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::SizeOfType,
  (twk::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::CharLiteral,
  (twk::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::BinOp,
  (twk::ast::Expr, lhs)
  (std::u32string, op)
  (twk::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::UnaryOp,
  (std::u32string, op)
  (twk::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Reference,
  (twk::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::New,
  (twk::ast::Type, type)
  (bool, with_init)
  (std::vector<twk::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Delete,
  (twk::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Dereference,
  (twk::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::MemberAccess,
  (twk::ast::Expr, lhs)
  (twk::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Subscript,
  (twk::ast::Expr, lhs)
  (twk::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::FunctionCall,
  (twk::ast::Expr, callee)
  (std::deque<twk::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::FunctionTemplateCall,
  (twk::ast::Expr, callee)
  (twk::ast::TemplateArguments, template_args)
  (std::deque<twk::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Cast,
  (twk::ast::Expr, lhs)
  (twk::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Pipeline,
  (twk::ast::Expr, lhs)
  (std::u32string, op)
  (twk::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ArrayLiteral,
  (std::vector<twk::ast::Expr>, elements)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ClassLiteral,
  (twk::ast::Type, type)
  (std::vector<twk::ast::Expr>, initializer_list)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::BuiltinMacro,
  (twk::codegen::BuiltinMacroKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ScopeResolution,
  (twk::ast::Expr, lhs)
  (twk::ast::Expr, rhs)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::VariableDef,
  (std::optional<twk::VariableQual>, qualifier)
  (twk::ast::Identifier, name)
  (std::optional<twk::ast::Type>, type)
  (std::optional<twk::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Return,
  (std::optional<twk::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Assignment,
  (twk::ast::Expr, lhs)
  (std::u32string, op)
  (twk::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ClassMemberInit,
  (twk::ast::Assignment, assign_ast)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::PrefixIncrementDecrement,
  (std::u32string, op)
  (twk::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::If,
  (twk::ast::Expr, condition)
  (twk::ast::Stmt, then_statement)
  (std::optional<twk::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Loop,
  (twk::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::While,
  (twk::ast::Expr, cond_expr)
  (twk::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::For,
  (std::optional<twk::ast::ForInitVariant>, init_stmt)
  (std::optional<twk::ast::Expr>, cond_expr)
  (std::optional<twk::ast::ForLoopVariant>, loop_stmt)
  (twk::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::MatchCase,
  (twk::ast::Expr, match_case)
  (twk::ast::Stmt, statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Match,
  (twk::ast::Expr, target)
  (twk::ast::MatchCaseList, cases)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ClassDecl,
  (twk::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::VariableDefWithoutInit,
  (std::optional<twk::VariableQual>, qualifier)
  (twk::ast::Identifier, name)
  (twk::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::MemberInitializer,
  (twk::ast::Identifier, member_name)
  (twk::ast::Expr, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::MemberInitializerList,
  (std::vector<twk::ast::MemberInitializer>, initializers)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Constructor,
	(twk::ast::FunctionDecl, decl)
  (twk::ast::MemberInitializerList, member_initializers)
  (twk::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Destructor,
	(twk::ast::FunctionDecl, decl)
  (twk::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ClassDef,
  (bool, is_public)
  (twk::ast::Identifier, name)
  (twk::ast::TemplateParameters, template_params)
  (twk::ast::ClassMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::UnionTag,
  (twk::ast::Identifier, tag_name)
  (twk::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::UnionDef,
  (bool, is_public)
  (twk::ast::Identifier, name)
  (twk::ast::TemplateParameters, template_params)
  (twk::ast::UnionTagList, type_list)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Parameter,
  (twk::ast::Identifier, name)
  (std::unordered_set<twk::VariableQual>, qualifier)
  (twk::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::ParameterList,
  (std::deque<twk::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::FunctionDecl,
  (twk::ast::Identifier, name)
  (twk::ast::TemplateParameters, template_params)
  (twk::ast::ParameterList, params)
  (twk::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::FunctionDef,
  (bool, is_public)
	(twk::ast::FunctionDecl, decl)
  (twk::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Typedef,
  (twk::ast::Identifier, alias)
  (twk::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Import,
  (twk::ast::Path, path)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::TopLevelWithAttr,
  (twk::ast::Attrs, attrs)
  (twk::ast::TopLevel, top_level)
)

BOOST_FUSION_ADAPT_STRUCT(
  twk::ast::Namespace,
  (twk::ast::Identifier, name)
  (twk::ast::TopLevelList, top_levels)
)

// clang-format on

#endif
