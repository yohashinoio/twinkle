/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _4a13c82a_9536_11ec_b909_0242ac120002
#define _4a13c82a_9536_11ec_b909_0242ac120002

#include <twinkle/pch/pch.hpp>
#include <twinkle/ast/ast.hpp>

// clang-format off

//===----------------------------------------------------------------------===//
// Common AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Identifier,
  (std::u32string, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Path,
  (std::u32string, path)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::TemplateParameters,
  (twinkle::ast::TemplateParameters::TypeNames, type_names)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::TemplateArguments,
  (twinkle::ast::TemplateArguments::Types, types)
)

//===----------------------------------------------------------------------===//
// Type AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::BuiltinType,
  (twinkle::codegen::BuiltinTypeKind, kind)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ArrayType,
  (twinkle::ast::Type, element_type)
  (std::uint64_t, size)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::PointerType,
  (std::vector<boost::blank>, n_ops)
  (twinkle::ast::Type, pointee_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::UserDefinedType,
  (twinkle::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::UserDefinedTemplateType,
  (twinkle::ast::UserDefinedType, template_type)
  (twinkle::ast::TemplateArguments, template_args)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ReferenceType,
  (twinkle::ast::Type, refee_type)
)

//===----------------------------------------------------------------------===//
// Expression AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::StringLiteral,
  (std::u32string, str)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::CharLiteral,
  (twinkle::unicode::Codepoint, ch)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::BinOp,
  (twinkle::ast::Expr, lhs)
  (std::u32string, op)
  (twinkle::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::UnaryOp,
  (std::u32string, op)
  (twinkle::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Reference,
  (twinkle::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::New,
  (twinkle::ast::Type, type)
  (bool, with_init)
  (std::vector<twinkle::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Delete,
  (twinkle::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Dereference,
  (twinkle::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::MemberAccess,
  (twinkle::ast::Expr, lhs)
  (twinkle::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Subscript,
  (twinkle::ast::Expr, lhs)
  (twinkle::ast::Expr, subscript)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::FunctionCall,
  (twinkle::ast::Expr, callee)
  (std::deque<twinkle::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::FunctionTemplateCall,
  (twinkle::ast::Expr, callee)
  (twinkle::ast::TemplateArguments, template_args)
  (std::deque<twinkle::ast::Expr>, args)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Cast,
  (twinkle::ast::Expr, lhs)
  (twinkle::ast::Type, as)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Pipeline,
  (twinkle::ast::Expr, lhs)
  (std::u32string, op)
  (twinkle::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ArrayLiteral,
  (std::vector<twinkle::ast::Expr>, elements)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ClassLiteral,
  (twinkle::ast::Type, type)
  (std::vector<twinkle::ast::Expr>, initializer_list)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::BuiltinMacro,
  (twinkle::codegen::BuiltinMacroKind, kind)
)

//===----------------------------------------------------------------------===//
// Statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::VariableDef,
  (std::optional<twinkle::VariableQual>, qualifier)
  (twinkle::ast::Identifier, name)
  (std::optional<twinkle::ast::Type>, type)
  (std::optional<twinkle::ast::Expr>, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Return,
  (std::optional<twinkle::ast::Expr>, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Assignment,
  (twinkle::ast::Expr, lhs)
  (std::u32string, op)
  (twinkle::ast::Expr, rhs)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ClassMemberInit,
  (twinkle::ast::Assignment, assign_ast)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::PrefixIncrementDecrement,
  (std::u32string, op)
  (twinkle::ast::Expr, operand)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Break,
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Continue,
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::If,
  (twinkle::ast::Expr, condition)
  (twinkle::ast::Stmt, then_statement)
  (std::optional<twinkle::ast::Stmt>, else_statement)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Loop,
  (twinkle::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::While,
  (twinkle::ast::Expr, cond_expr)
  (twinkle::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::For,
  (std::optional<twinkle::ast::ForInitVariant>, init_stmt)
  (std::optional<twinkle::ast::Expr>, cond_expr)
  (std::optional<twinkle::ast::ForLoopVariant>, loop_stmt)
  (twinkle::ast::Stmt, body)
)

//===----------------------------------------------------------------------===//
// Top level statement AST adapt
//===----------------------------------------------------------------------===//

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ClassDecl,
  (twinkle::ast::Identifier, name)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::VariableDefWithoutInit,
  (std::optional<twinkle::VariableQual>, qualifier)
  (twinkle::ast::Identifier, name)
  (twinkle::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::MemberInitializer,
  (twinkle::ast::Identifier, member_name)
  (twinkle::ast::Expr, initializer)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::MemberInitializerList,
  (std::vector<twinkle::ast::MemberInitializer>, initializers)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Constructor,
	(twinkle::ast::FunctionDecl, decl)
  (twinkle::ast::MemberInitializerList, member_initializers)
  (twinkle::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Destructor,
	(twinkle::ast::FunctionDecl, decl)
  (twinkle::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ClassDef,
  (bool, is_public)
  (twinkle::ast::Identifier, name)
  (twinkle::ast::TemplateParameters, template_params)
  (twinkle::ast::ClassMemberList, members)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Parameter,
  (twinkle::ast::Identifier, name)
  (std::unordered_set<twinkle::VariableQual>, qualifier)
  (twinkle::ast::Type, type)
  (bool, is_vararg)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::ParameterList,
  (std::deque<twinkle::ast::Parameter>, params)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::FunctionDecl,
  (twinkle::ast::Identifier, name)
  (twinkle::ast::TemplateParameters, template_params)
  (twinkle::ast::ParameterList, params)
  (twinkle::ast::Type, return_type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::FunctionDef,
  (bool, is_public)
	(twinkle::ast::FunctionDecl, decl)
  (twinkle::ast::Stmt, body)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Typedef,
  (twinkle::ast::Identifier, alias)
  (twinkle::ast::Type, type)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::Import,
  (twinkle::ast::Path, path)
)

BOOST_FUSION_ADAPT_STRUCT(
  twinkle::ast::TopLevelWithAttr,
  (twinkle::ast::Attrs, attrs)
  (twinkle::ast::TopLevel, top_level)
)

// clang-format on

#endif
