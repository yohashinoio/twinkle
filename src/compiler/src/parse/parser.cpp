/**
 * These codes are licensed under MIT License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/pch/pch.hpp>
#include <twinkle/ast/ast_adapted.hpp>
#include <twinkle/parse/parser.hpp>
#include <twinkle/codegen/type.hpp>
#include <twinkle/codegen/kind.hpp>
#include <twinkle/parse/exception.hpp>

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace twinkle::parse
{

//===----------------------------------------------------------------------===//
// Error handling
//===----------------------------------------------------------------------===//

struct ErrorHandle {
  template <typename Iterator, typename Context>
  x3::error_handler_result on_error(Iterator&,
                                    const Iterator&,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    auto& error_handler = x3::get<x3::error_handler_tag>(context).get();

    error_handler(
      x.where(),
      formatError("expected: " + boost::core::demangle(x.which().c_str())));

    return x3::error_handler_result::fail;
  }
};

//===----------------------------------------------------------------------===//
// Annotations
//===----------------------------------------------------------------------===//

// Tag used to get the position cache from the context.
struct PositionCacheTag;

struct AnnotatePosition {
  template <typename T, typename Iterator, typename Context>
  void on_success(const Iterator& first,
                  const Iterator& last,
                  T&              ast,
                  const Context&  ctx)
  {
    auto&& position_cache = x3::get<PositionCacheTag>(ctx);
    position_cache.annotate(ast, first, last);
  }
};

//===----------------------------------------------------------------------===//
// Semantic actions
//===----------------------------------------------------------------------===//

namespace action
{

const auto assignAttrToVal = [](const auto& ctx) {
  x3::_val(ctx) = std::move(x3::_attr(ctx));
};

template <typename T>
struct assignToValAs {
  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const
    // clang-format off
    -> std::enable_if_t<
         std::is_same_v<Ast, ast::BinOp>
      || std::is_same_v<Ast, ast::Pipeline>
    >
  // clang-format on
  {
    assignAstToVal(ctx,
                   Ast{std::move(x3::_val(ctx)),
                       std::move(fusion::at_c<0>(x3::_attr(ctx))),
                       std::move(fusion::at_c<1>(x3::_attr(ctx)))});
  }

  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const
    // clang-format off
    -> std::enable_if_t<
         std::is_same_v<Ast, ast::Cast>
      || std::is_same_v<Ast, ast::Subscript>
      || std::is_same_v<Ast, ast::FunctionCall>
      || std::is_same_v<Ast, ast::MemberAccess>
      || std::is_same_v<Ast, ast::ScopeResolution>
    >
  // clang-format on
  {
    assignAstToVal(ctx,
                   Ast{
                     std::move(x3::_val(ctx)),
                     std::move(fusion::at_c<1>(x3::_attr(ctx))),
                   });
  };

  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const
    -> std::enable_if_t<std::is_same_v<Ast, ast::ArrayType>>
  {
    assignAstToVal(ctx,
                   Ast{
                     std::move(x3::_val(ctx)),
                     std::move(fusion::at_c<1>(x3::_attr(ctx))),
                   });
  };

  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const
    -> std::enable_if_t<std::is_same_v<Ast, ast::Dereference>>
  {
    assignAstToVal(ctx, Ast{std::move(x3::_val(ctx))});
  }

  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const
    -> std::enable_if_t<std::is_same_v<Ast, ast::FunctionTemplateCall>>
  {
    assignAstToVal(ctx,
                   Ast{std::move(x3::_val(ctx)),
                       std::move(fusion::at_c<0>(x3::_attr(ctx))),
                       std::move(fusion::at_c<2>(x3::_attr(ctx)))});
  }

private:
  template <typename Ctx>
  void assignAstToVal(const Ctx& ctx, T&& ast) const
  {
    // FIXME: There is a bug that causes the position to be annotated one
    // position further than the actual position.
    auto&& position_cache = x3::get<PositionCacheTag>(ctx);
    position_cache.annotate(ast,
                            x3::_where(ctx).begin(),
                            x3::_where(ctx).end());

    x3::_val(ctx) = std::forward<T>(ast);
  }
};

} // namespace action

//===----------------------------------------------------------------------===//
// Syntax
//===----------------------------------------------------------------------===//

namespace syntax
{

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-shift-op-parentheses"

using x3::unicode::lit;
using x3::unicode::char_;
using x3::unicode::string;

template <typename T>
using UnicodeSymbols
  = x3::symbols_parser<boost::spirit::char_encoding::unicode, T>;

// The reason for using x3::rule where a rule is not a recursive rule is to
// speed up compilation.

//===----------------------------------------------------------------------===//
// Symbol table
//===----------------------------------------------------------------------===//

struct VariableQualifierSymbols : UnicodeSymbols<VariableQual> {
  VariableQualifierSymbols()
  {
    // clang-format off
    add
      (U"mut", VariableQual::mutable_)
    ;
    // clang-format on
  }
} variable_qualifier_symbols;

struct AccessSpecifierSymbols : UnicodeSymbols<Accessibility> {
  AccessSpecifierSymbols()
  {
    // clang-format off
    add
      (U"public", Accessibility::public_)
      (U"private", Accessibility::private_)
    ;
    // clang-format on
  }
} access_specifier_symbols;

struct EscapeCharSymbols : UnicodeSymbols<char32_t> {
  EscapeCharSymbols()
  {
    // clang-format off
    add
      (U"\\a", U'\a')
      (U"\\b", U'\b')
      (U"\\f", U'\f')
      (U"\\n", U'\n')
      (U"\\r", U'\r')
      (U"\\t", U'\t')
      (U"\\v", U'\v')
      (U"\\0", U'\0')
      (U"\\\\", U'\\')
      (U"\\\'", U'\'')
      (U"\\\"", U'\"')
    ;
    // clang-format on
  }
} escape_char_symbols;

struct BuiltinTypeNameSymbolsTag : UnicodeSymbols<codegen::BuiltinTypeKind> {
  BuiltinTypeNameSymbolsTag()
  {
    // clang-format off
    add
      (U"void", codegen::BuiltinTypeKind::void_)
      (U"i8", codegen::BuiltinTypeKind::i8)
      (U"i16", codegen::BuiltinTypeKind::i16)
      (U"i32", codegen::BuiltinTypeKind::i32)
      (U"i64", codegen::BuiltinTypeKind::i64)
      (U"u8", codegen::BuiltinTypeKind::u8)
      (U"u16", codegen::BuiltinTypeKind::u16)
      (U"u32", codegen::BuiltinTypeKind::u32)
      (U"u64", codegen::BuiltinTypeKind::u64)
      (U"bool", codegen::BuiltinTypeKind::bool_)
      (U"char", codegen::BuiltinTypeKind::char_)
      (U"f64", codegen::BuiltinTypeKind::f64)
      (U"f32", codegen::BuiltinTypeKind::f32)
      (U"isize", codegen::BuiltinTypeKind::isize)
      (U"usize", codegen::BuiltinTypeKind::usize)
    ;
    // clang-format on
  }
} builtin_type_symbols;

struct BuiltinMacroSymbols : UnicodeSymbols<codegen::BuiltinMacroKind> {
  BuiltinMacroSymbols()
  {
    // clang-format off
    add
      (U"__builtin_huge_valf", codegen::BuiltinMacroKind::huge_valf)
      (U"__builtin_huge_val", codegen::BuiltinMacroKind::huge_val)
      (U"__builtin_infinity", codegen::BuiltinMacroKind::infinity_)
    ;
    // clang-format on
  }
} builtin_macro_symbols;

//===----------------------------------------------------------------------===//
// Macro for rule declarations
//===----------------------------------------------------------------------===//

#define DECLARE_X3_RULE(name, ast_type, alt)             \
  const x3::rule<struct name##_tag, ast_type> name{alt}; \
  struct name##_tag                                      \
    : ErrorHandle                                        \
    , AnnotatePosition {};

#define DECLARE_X3_RULE_NO_ATTR(name, alt)     \
  const x3::rule<struct name##_tag> name{alt}; \
  struct name##_tag;

//===----------------------------------------------------------------------===//
// Type rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE(builtin_type, ast::BuiltinType, "builtin type name")
DECLARE_X3_RULE(reference_type_internal, ast::ReferenceType, "reference type")
DECLARE_X3_RULE(reference_type, ast::Type, "reference type")
DECLARE_X3_RULE(array_type, ast::Type, "array type")
DECLARE_X3_RULE(pointer_type_internal, ast::PointerType, "pointer type")
DECLARE_X3_RULE(pointer_type, ast::Type, "pointer type")
DECLARE_X3_RULE(user_defined_type, ast::UserDefinedType, "user defined type")
DECLARE_X3_RULE(user_defined_template_type,
                ast::UserDefinedTemplateType,
                "user defined template type")
DECLARE_X3_RULE(type_primary, ast::Type, "type primary")

//===----------------------------------------------------------------------===//
// Common rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE_NO_ATTR(punct, "punctuation character")
DECLARE_X3_RULE(identifier_internal, std::u32string, "identifier")
DECLARE_X3_RULE(identifier, ast::Identifier, "identifier")
DECLARE_X3_RULE(path_internal, std::u32string, "path")
DECLARE_X3_RULE(path, ast::Path, "path")
DECLARE_X3_RULE(variable_qualifier, VariableQual, "variable qualifier")
DECLARE_X3_RULE(access_specifier, Accessibility, "access specifier")
DECLARE_X3_RULE(binary_literal, std::uint32_t, "binary literal")
DECLARE_X3_RULE(octal_literal, std::uint32_t, "octal literal")
DECLARE_X3_RULE(hex_literal, std::uint32_t, "hexadecimal literal")
DECLARE_X3_RULE(uint_32bit, std::uint32_t, "integral number")
DECLARE_X3_RULE(int_32bit, std::int32_t, "integral number")
DECLARE_X3_RULE(uint_64bit, std::uint64_t, "integral number (64bit)")
DECLARE_X3_RULE(int_64bit, std::int64_t, "integral number (64bit)")
DECLARE_X3_RULE(float_64bit, double, "double precision floating point number")
DECLARE_X3_RULE(boolean_literal, bool, "boolean literal")
DECLARE_X3_RULE(escape_char, unsigned char, "escape character")
DECLARE_X3_RULE(string_literal, ast::StringLiteral, "string literal")
DECLARE_X3_RULE(char_literal, ast::CharLiteral, "character literal")
DECLARE_X3_RULE(attribute, ast::Attrs, "attribute")
DECLARE_X3_RULE(builtin_macro, ast::BuiltinMacro, "builtin macro")
DECLARE_X3_RULE_NO_ATTR(space, "space")
DECLARE_X3_RULE(array_literal, ast::ArrayLiteral, "array literal")
DECLARE_X3_RULE(class_literal, ast::ClassLiteral, "class literal")
DECLARE_X3_RULE(template_args, ast::TemplateArguments, "template arguments")

//===----------------------------------------------------------------------===//
// Type rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE(type_name, ast::Type, "type name")

//===----------------------------------------------------------------------===//
// Expression rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE(expr, ast::Expr, "expression")
DECLARE_X3_RULE(binary_logical, ast::Expr, "binary logical operation")
DECLARE_X3_RULE(equal, ast::Expr, "equality operation")
DECLARE_X3_RULE(relation, ast::Expr, "relational operation")
DECLARE_X3_RULE(bitwise_and, ast::Expr, "bitwise and operation")
DECLARE_X3_RULE(bitwise_or, ast::Expr, "bitwise or operation")
DECLARE_X3_RULE(pipeline, ast::Expr, "pipeline operation")
DECLARE_X3_RULE(bitwise_shift, ast::Expr, "bitwise shift operation")
DECLARE_X3_RULE(add, ast::Expr, "addition operation")
DECLARE_X3_RULE(mul, ast::Expr, "multiplication operation")
DECLARE_X3_RULE(cast, ast::Expr, "cast opeartion")
DECLARE_X3_RULE(unary_internal, ast::UnaryOp, "unary operation")
DECLARE_X3_RULE(unary, ast::Expr, "unary operation")
DECLARE_X3_RULE(reference_internal, ast::Reference, "reference operation")
DECLARE_X3_RULE(reference, ast::Expr, "reference operation")
DECLARE_X3_RULE(new_internal, ast::New, "new operation")
DECLARE_X3_RULE(new_, ast::Expr, "new operation")
DECLARE_X3_RULE(delete_internal, ast::Delete, "delete operation")
DECLARE_X3_RULE(delete_, ast::Expr, "delete operation")
DECLARE_X3_RULE(dereference, ast::Expr, "dereference operation")
DECLARE_X3_RULE(member_access, ast::Expr, "member access operation")
DECLARE_X3_RULE(subscript, ast::Expr, "subscript operation")
DECLARE_X3_RULE(arg_list, std::deque<ast::Expr>, "argument list")
DECLARE_X3_RULE(function_call, ast::Expr, "function call")
DECLARE_X3_RULE(function_template_call, ast::Expr, "function template call")
DECLARE_X3_RULE(scope_resolution, ast::Expr, "scope resolution")
DECLARE_X3_RULE(size_of_type, ast::SizeOfType, "size of type")
DECLARE_X3_RULE(null_pointer, ast::NullPointer, "null pointer")
DECLARE_X3_RULE(primary, ast::Expr, "primary")

//===----------------------------------------------------------------------===//
// Statement rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE(expr_stmt, ast::Expr, "expression statement")
DECLARE_X3_RULE(variable_def, ast::VariableDef, "variable definition")
DECLARE_X3_RULE(assignment, ast::Assignment, "assignment statement")
DECLARE_X3_RULE(prefix_increment_decrement,
                ast::PrefixIncrementDecrement,
                "prefix increment/decrement")
DECLARE_X3_RULE(_return, ast::Return, "return statement")
DECLARE_X3_RULE(_if, ast::If, "if else statement")
DECLARE_X3_RULE(_loop, ast::Loop, "loop statement")
DECLARE_X3_RULE(_while, ast::While, "while statement")
DECLARE_X3_RULE(_for, ast::For, "for statement")
DECLARE_X3_RULE(_break, ast::Break, "break statement")
DECLARE_X3_RULE(_continue, ast::Continue, "continue statement")
DECLARE_X3_RULE(match_case, ast::MatchCase, "match statement case")
DECLARE_X3_RULE(match, ast::Match, "match statement")
DECLARE_X3_RULE(stmt, ast::Stmt, "statement")

//===----------------------------------------------------------------------===//
// Top level rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE(is_public, bool, "public")
DECLARE_X3_RULE(template_params, ast::TemplateParameters, "template parameters")
DECLARE_X3_RULE_NO_ATTR(class_key, "class key")
DECLARE_X3_RULE_NO_ATTR(union_key, "union key")
DECLARE_X3_RULE(class_decl, ast::ClassDecl, "class declaration")
DECLARE_X3_RULE(variable_def_without_init,
                ast::VariableDefWithoutInit,
                "variable definition without initializer")
DECLARE_X3_RULE(member_initializer,
                ast::MemberInitializer,
                "member initializer")
DECLARE_X3_RULE(member_initializer_list,
                ast::MemberInitializerList,
                "member initializer list")
DECLARE_X3_RULE(constructor, ast::Constructor, "constructor")
DECLARE_X3_RULE(destructor, ast::Destructor, "destructor")
DECLARE_X3_RULE(class_member_list, ast::ClassMemberList, "class member list")
DECLARE_X3_RULE(class_def, ast::ClassDef, "class definition")
DECLARE_X3_RULE(union_tag, ast::UnionTag, "union tag")
DECLARE_X3_RULE(union_tag_list, ast::UnionTagList, "union tag list")
DECLARE_X3_RULE(union_def, ast::UnionDef, "union definition")
DECLARE_X3_RULE(parameter, ast::Parameter, "parameter")
DECLARE_X3_RULE(parameter_list, ast::ParameterList, "parameter list")
DECLARE_X3_RULE(function_proto, ast::FunctionDecl, "function prototype")
DECLARE_X3_RULE(function_decl, ast::FunctionDecl, "function declaration")
DECLARE_X3_RULE(function_def, ast::FunctionDef, "function definition")
DECLARE_X3_RULE(type_def, ast::Typedef, "typedef")
DECLARE_X3_RULE(import_, ast::Import, "import")
DECLARE_X3_RULE(name_space, ast::Namespace, "namespace")
DECLARE_X3_RULE(top_level, ast::TopLevel, "top level")
DECLARE_X3_RULE(top_level_with_attr, ast::TopLevelWithAttr, "top level")
DECLARE_X3_RULE(top_level_list, ast::TopLevelList, "top level list")

//===----------------------------------------------------------------------===//
// Comment rules declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE_NO_ATTR(single_line_comment, "single line comment")
DECLARE_X3_RULE_NO_ATTR(block_comment, "block comment")
DECLARE_X3_RULE_NO_ATTR(comment, "comment")

//===----------------------------------------------------------------------===//
// Skipper rule declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE_NO_ATTR(skipper, "skipper")

//===----------------------------------------------------------------------===//
// Translation unit rule declaration
//===----------------------------------------------------------------------===//

DECLARE_X3_RULE(translation_unit, ast::TranslationUnit, "translation unit")

//===----------------------------------------------------------------------===//
// Undef macros
//===----------------------------------------------------------------------===//

#undef DECLARE_X3_RULE
#undef DECLARE_X3_RULE_NO_ATTR

//===----------------------------------------------------------------------===//
// Common rules definition
//===----------------------------------------------------------------------===//

const auto punct_def
  = x3::unicode::punct | lit(U"^") | lit(U"\"") | lit(U"<") | lit(U">");

const auto identifier_internal_def
  = x3::raw[x3::lexeme[(x3::unicode::graph - (x3::unicode::digit | punct)
                        | lit(U"_"))
                       >> *((x3::unicode::graph - punct) | lit(U"_"))]];

const auto identifier_def
  = identifier_internal - (lit(U"true") | lit(U"false") | lit(U"nullptr"));

const auto path_internal_def
  = x3::raw[x3::lexeme[(x3::unicode::graph - (x3::unicode::digit | punct)
                        | lit(U"_") | lit(U".") | lit(U"/"))
                       >> *(x3::unicode::graph - punct | lit(U"_") | lit(U".")
                            | lit(U"/"))]];

const auto path_def = path_internal;

const auto variable_qualifier_def = variable_qualifier_symbols;

const auto access_specifier_def = access_specifier_symbols;

const auto binary_literal_def
  = x3::lexeme[lit(U"0b") >> x3::uint_parser<std::uint32_t, 2>{}];

const auto octal_literal_def
  = x3::lexeme[lit(U"0") >> x3::uint_parser<std::uint32_t, 8>{}];

const auto hex_literal_def
  = x3::lexeme[lit(U"0x") >> x3::uint_parser<std::uint32_t, 16>{}];

const auto uint_32bit_def = x3::uint32;

const auto int_32bit_def = x3::int32;

const auto uint_64bit_def = x3::uint64;

const auto int_64bit_def = x3::int64;

const auto float_64bit_def = boost::spirit::x3::
  real_parser<double, boost::spirit::x3::strict_real_policies<double>>{};

const auto boolean_literal_def
  = lit(U"true") >> x3::attr(true) | lit(U"false") >> x3::attr(false);

const auto escape_char_def
  = lit(U"\\") >> x3::int_parser<char, 8, 1, 3>{}     // Octal
    | lit(U"\\x") >> x3::int_parser<char, 16, 2, 2>{} // Hexadecimal
    | escape_char_symbols;

const auto string_literal_def
  = x3::lexeme[lit(U"\"")
               >> *(char_ - (lit(U"\"") | x3::eol | lit(U"\\")) | escape_char)
               > lit(U"\"")];

const auto char_literal_def
  = lit(U"'") >> (char_ - (lit(U"'") | x3::eol | lit(U"\\")) | escape_char)
    > lit(U"'");

const auto attribute_def
  = lit(U"[[") >> (identifier_internal % lit(U",")) > lit(U"]]");

// Do not use the expectation operator because it may be a comparison
// operation
// (< or >)
const auto template_args_def
  = lit(U"<") >> (type_name % lit(U",")) >> lit(U">");

const auto array_literal_def = lit(U"[") > (expr % lit(U",")) > lit(U"]");

const auto class_literal_def
  = type_name >> lit(U"{") > -(expr % lit(U",")) > lit(U"}");

const auto builtin_macro_def = builtin_macro_symbols;

const auto space_def = x3::unicode::space;

BOOST_SPIRIT_DEFINE(punct)
BOOST_SPIRIT_DEFINE(identifier_internal)
BOOST_SPIRIT_DEFINE(identifier)
BOOST_SPIRIT_DEFINE(path_internal)
BOOST_SPIRIT_DEFINE(path)
BOOST_SPIRIT_DEFINE(variable_qualifier)
BOOST_SPIRIT_DEFINE(access_specifier)
BOOST_SPIRIT_DEFINE(binary_literal)
BOOST_SPIRIT_DEFINE(octal_literal)
BOOST_SPIRIT_DEFINE(hex_literal)
BOOST_SPIRIT_DEFINE(uint_32bit)
BOOST_SPIRIT_DEFINE(int_32bit)
BOOST_SPIRIT_DEFINE(uint_64bit)
BOOST_SPIRIT_DEFINE(int_64bit)
BOOST_SPIRIT_DEFINE(float_64bit)
BOOST_SPIRIT_DEFINE(boolean_literal)
BOOST_SPIRIT_DEFINE(escape_char)
BOOST_SPIRIT_DEFINE(string_literal)
BOOST_SPIRIT_DEFINE(char_literal)
BOOST_SPIRIT_DEFINE(attribute)
BOOST_SPIRIT_DEFINE(builtin_macro)
BOOST_SPIRIT_DEFINE(space)
BOOST_SPIRIT_DEFINE(array_literal)
BOOST_SPIRIT_DEFINE(class_literal)
BOOST_SPIRIT_DEFINE(template_args)

//===----------------------------------------------------------------------===//
// Type name rules definition
//===----------------------------------------------------------------------===//

const auto builtin_type_def = builtin_type_symbols;

const auto type_name_def = reference_type;

const auto reference_type_internal_def = lit(U"&") > array_type;

const auto reference_type_def = array_type | reference_type_internal;

const auto array_type_def
  = pointer_type[action::assignAttrToVal]
    >> *(string(U"[") >> uint_64bit
         >> lit(U"]"))[action::assignToValAs<ast::ArrayType>{}];

const auto pointer_type_internal_def
  = +(lit(U"^") > x3::attr(boost::blank{})) > type_primary;

const auto pointer_type_def = type_primary | pointer_type_internal;

const auto user_defined_type_def = identifier;

const auto user_defined_template_type_def = user_defined_type >> template_args;

const auto type_primary_def = builtin_type | user_defined_template_type
                              | user_defined_type
                              | (lit(U"(") >> type_name >> lit(U")"));

BOOST_SPIRIT_DEFINE(builtin_type)
BOOST_SPIRIT_DEFINE(type_name)
BOOST_SPIRIT_DEFINE(array_type)
BOOST_SPIRIT_DEFINE(reference_type_internal)
BOOST_SPIRIT_DEFINE(reference_type)
BOOST_SPIRIT_DEFINE(pointer_type_internal)
BOOST_SPIRIT_DEFINE(pointer_type)
BOOST_SPIRIT_DEFINE(user_defined_type)
BOOST_SPIRIT_DEFINE(user_defined_template_type)
BOOST_SPIRIT_DEFINE(type_primary)

//===----------------------------------------------------------------------===//
// Operator rules definition
//===----------------------------------------------------------------------===//

const auto assignment_operator = x3::rule<struct AssignmentOperatorTag,
                                          std::u32string>{"assignment operator"}
= string(U"=") | string(U"+=") | string(U"-=") | string(U"*=") | string(U"/=")
  | string(U"%=");

const auto equality_operator
  = x3::rule<struct EqualityOperatorTag, std::u32string>{"equality operator"}
= string(U"==") | string(U"!=");

const auto relational_operator = x3::rule<struct RelationalOperatorTag,
                                          std::u32string>{"relational operator"}
= string(U"<=") | string(U">=") /* <= and >= must come first */
  | string(U"<") | string(U">");

const auto additive_operator
  = x3::rule<struct AdditiveOperatorTag, std::u32string>{"additive operator"}
= (string(U"+") - string(U"+=")) | (string(U"-") - string(U"-="));

const auto pipeline_operator
  = x3::rule<struct PipelineOperatorTag, std::u32string>{"pipeline operator"}
= string(U"|>");

const auto multitive_operator
  = x3::rule<struct MultitiveOperatorTag, std::u32string>{"multitive operator"}
= (string(U"*") - string(U"*=")) | (string(U"/") - string(U"/="))
  | (string(U"%") - string(U"%="));

// logical not is excluded because it is a unary operator.
const auto binary_logical_operator
  = x3::rule<struct BinaryLogicalOperatorTag,
             std::u32string>{"binary logical operator"}
= string(U"&&") | string(U"||");

const auto unary_operator
  = x3::rule<struct UnaryOperatorTag, std::u32string>{"unary operator"}
= string(U"+") | string(U"-") | string(U"!") | string(U"*") | string(U"&")
  | string(U"sizeof");

const auto bitwise_shift_operator
  = x3::rule<struct BitwiseShiftOperatorTag,
             std::u32string>{"bitwise shift operator"}
= string(U"<<") | string(U">>");

const auto bitwise_or_operator
  = x3::rule<struct BitwiseOrOperatorTag, std::u32string>{"bitwise OR operator"}
= string(U"|") - lit(U"||");

const auto bitwise_and_operator
  = x3::rule<struct BitwiseAndOperatorTag,
             std::u32string>{"bitwise AND operator"}
= string(U"&") - lit(U"&&");

//===----------------------------------------------------------------------===//
// Expression rules definition
//===----------------------------------------------------------------------===//

const auto size_of_type_def = type_name >> lit(U".") >> lit(U"sizeof");

const auto expr_def = binary_logical;

const auto binary_logical_def
  = equal[action::assignAttrToVal]
    >> *(binary_logical_operator > equal)[action::assignToValAs<ast::BinOp>{}];

const auto equal_def
  = relation[action::assignAttrToVal]
    >> *(equality_operator > relation)[action::assignToValAs<ast::BinOp>{}];

const auto relation_def
  = bitwise_or[action::assignAttrToVal]
    >> *(relational_operator > bitwise_or)[action::assignToValAs<ast::BinOp>{}];

const auto bitwise_or_def
  = bitwise_and[action::assignAttrToVal]
    >> *(bitwise_or_operator
         > bitwise_and)[action::assignToValAs<ast::BinOp>{}];

const auto bitwise_and_def
  = pipeline[action::assignAttrToVal]
    >> *(bitwise_and_operator > pipeline)[action::assignToValAs<ast::BinOp>{}];

const auto pipeline_def
  = bitwise_shift[action::assignAttrToVal]
    >> *(pipeline_operator
         > bitwise_shift)[action::assignToValAs<ast::Pipeline>{}];

const auto bitwise_shift_def
  = add[action::assignAttrToVal]
    >> *(bitwise_shift_operator > add)[action::assignToValAs<ast::BinOp>{}];

const auto add_def
  = mul[action::assignAttrToVal]
    >> *(additive_operator > mul)[action::assignToValAs<ast::BinOp>{}];

const auto mul_def
  = cast[action::assignAttrToVal]
    >> *(multitive_operator > cast)[action::assignToValAs<ast::BinOp>{}];

const auto cast_def
  = unary[action::assignAttrToVal]
    >> *(string(U"as") > type_name)[action::assignToValAs<ast::Cast>{}];

const auto unary_internal_def = unary_operator >> reference;
const auto unary_def          = unary_internal | reference;

const auto reference_internal_def = lit(U"ref") >> x3::no_skip[space] > new_;
const auto reference_def          = reference_internal | new_;

const auto new_internal_def = lit(U"new") >> x3::no_skip[space] > type_name
                              > x3::matches[lit(U"{")]
                              > -(expr % lit(U",") > lit(U"}"));
const auto new__def = new_internal | delete_;

const auto delete_internal_def = lit(U"delete") >> x3::no_skip[space] > expr;
const auto delete__def         = delete_internal | member_access;

const auto member_access_def
  = subscript[action::assignAttrToVal]
    >> *(string(U".") > subscript)[action::assignToValAs<ast::MemberAccess>{}];

const auto subscript_def
  = dereference[action::assignAttrToVal]
    >> *(string(U"[") > expr
         > lit(U"]"))[action::assignToValAs<ast::Subscript>{}];

const auto dereference_def
  = scope_resolution[action::assignAttrToVal]
    >> *lit(U"^")[action::assignToValAs<ast::Dereference>{}];

const auto scope_resolution_def
  = function_call[action::assignAttrToVal]
    >> *(string(U"::")
         > function_call)[action::assignToValAs<ast::ScopeResolution>{}];

const auto arg_list_def = -(expr % lit(U","));

const auto function_call_def
  = function_template_call[action::assignAttrToVal]
    >> *(string(U"(") > arg_list
         > lit(U")"))[action::assignToValAs<ast::FunctionCall>{}];

const auto function_template_call_def
  = primary[action::assignAttrToVal]
    >> *(template_args > string(U"(") > arg_list
         > lit(U")"))[action::assignToValAs<ast::FunctionTemplateCall>{}];

const auto null_pointer_def = lit(U"nullptr") >> x3::attr(ast::NullPointer{});

const auto primary_def
  = null_pointer | builtin_macro | size_of_type | class_literal | identifier
    | float_64bit | binary_literal | octal_literal | hex_literal | int_32bit
    | uint_32bit | int_64bit | uint_64bit | boolean_literal | string_literal
    | char_literal | array_literal | template_args
    | (lit(U"(") > expr > lit(U")"));

BOOST_SPIRIT_DEFINE(size_of_type)
BOOST_SPIRIT_DEFINE(expr)
BOOST_SPIRIT_DEFINE(binary_logical)
BOOST_SPIRIT_DEFINE(equal)
BOOST_SPIRIT_DEFINE(relation)
BOOST_SPIRIT_DEFINE(bitwise_and)
BOOST_SPIRIT_DEFINE(bitwise_or)
BOOST_SPIRIT_DEFINE(pipeline)
BOOST_SPIRIT_DEFINE(bitwise_shift)
BOOST_SPIRIT_DEFINE(add)
BOOST_SPIRIT_DEFINE(mul)
BOOST_SPIRIT_DEFINE(cast)
BOOST_SPIRIT_DEFINE(unary)
BOOST_SPIRIT_DEFINE(reference_internal)
BOOST_SPIRIT_DEFINE(reference)
BOOST_SPIRIT_DEFINE(new_internal)
BOOST_SPIRIT_DEFINE(new_)
BOOST_SPIRIT_DEFINE(delete_internal)
BOOST_SPIRIT_DEFINE(delete_)
BOOST_SPIRIT_DEFINE(dereference)
BOOST_SPIRIT_DEFINE(member_access)
BOOST_SPIRIT_DEFINE(subscript)
BOOST_SPIRIT_DEFINE(arg_list)
BOOST_SPIRIT_DEFINE(function_call)
BOOST_SPIRIT_DEFINE(function_template_call)
BOOST_SPIRIT_DEFINE(unary_internal)
BOOST_SPIRIT_DEFINE(scope_resolution)
BOOST_SPIRIT_DEFINE(null_pointer)
BOOST_SPIRIT_DEFINE(primary)

//===----------------------------------------------------------------------===//
// Statement rules definition
//===----------------------------------------------------------------------===//

const auto expr_stmt_def = expr;

const auto assignment_def = expr >> assignment_operator > expr;

const auto prefix_increment_decrement_def
  = (string(U"++") | string(U"--")) > expr;

const auto variable_def_def = lit(U"let") > -variable_qualifier > identifier
                              > -(lit(U":") > type_name) > -(lit(U"=") > expr);

const auto _return_def = lit(U"return") > -expr;

const auto _if_def
  = lit(U"if") > lit(U"(") > expr > lit(U")") > stmt > -(lit(U"else") > stmt);

const auto _loop_def = lit(U"loop") > stmt;

const auto _while_def = lit(U"while") > lit(U"(") > expr /* Condition */
                        > lit(U")") > stmt;

const auto _for_def
  = lit(U"for") > lit(U"(") > -(assignment | variable_def)   /* Init */
    > lit(U";") > -expr                                      /* Condition */
    > lit(U";") > -(prefix_increment_decrement | assignment) /* Loop */
    > lit(U")") > stmt;

const auto _break_def = lit(U"break") >> x3::attr(ast::Break{});

const auto _continue_def = lit(U"continue") >> x3::attr(ast::Continue{});

const auto match_case_def = expr > lit(U"=>") > stmt;

const auto match_def
  = expr >> lit(U"match") > lit(U"{") > *match_case > lit(U"}");

const auto stmt_def
  = lit(U";")                       /* Null statement */
    | lit(U"{") > *stmt > lit(U"}") /* Compound statement */
    | _loop | _while | _for | _if | match | _break >> lit(U";")
    | _continue >> lit(U";") | _return >> lit(U";")
    | prefix_increment_decrement >> lit(U";") | assignment >> lit(U";")
    | variable_def >> lit(U";") | expr_stmt >> lit(U";");

BOOST_SPIRIT_DEFINE(expr_stmt)
BOOST_SPIRIT_DEFINE(variable_def)
BOOST_SPIRIT_DEFINE(assignment)
BOOST_SPIRIT_DEFINE(prefix_increment_decrement)
BOOST_SPIRIT_DEFINE(_return)
BOOST_SPIRIT_DEFINE(_if)
BOOST_SPIRIT_DEFINE(_loop)
BOOST_SPIRIT_DEFINE(_while)
BOOST_SPIRIT_DEFINE(_for)
BOOST_SPIRIT_DEFINE(_break)
BOOST_SPIRIT_DEFINE(_continue)
BOOST_SPIRIT_DEFINE(match_case)
BOOST_SPIRIT_DEFINE(match)
BOOST_SPIRIT_DEFINE(stmt)

//===----------------------------------------------------------------------===//
// Top level rules definition
//===----------------------------------------------------------------------===//

const auto is_public_def = x3::matches[lit(U"pub")];

const auto template_params_def
  = -(lit(U"<") > (identifier % lit(U",")) > lit(U">"));

const auto class_key_def = lit(U"class");

const auto union_key_def = lit(U"union");

const auto class_decl_def
  = lit(U"declare") >> class_key > identifier > lit(U";");

const auto variable_def_without_init_def
  = lit(U"let") > -variable_qualifier > identifier > lit(U":") > type_name;

const auto member_initializer_def = identifier > lit(U"{") > expr > lit(U"}");

const auto member_initializer_list_def
  = lit(U":") > (member_initializer % lit(U","));

const auto constructor_def = function_proto > -member_initializer_list > stmt;

const auto destructor_def = lit(U"~") >> function_proto > stmt;

const auto class_member_list_def
  = *((variable_def_without_init > lit(U";")) | (access_specifier > lit(U":"))
      | class_def | function_def | destructor | constructor);

const auto class_def_def = is_public >> class_key > identifier > template_params
                           > lit(U"{") > class_member_list > lit(U"}");

const auto union_tag_def = identifier > lit(U"(") > type_name > lit(U")");

const auto union_tag_list_def = (union_tag % lit(U",")) > -lit(U",");

const auto union_def_def = is_public >> union_key > identifier > template_params
                           > lit(U"{") > union_tag_list > lit(U"}");

const auto parameter_def
  = (identifier > lit(U":") > *variable_qualifier > type_name > x3::attr(false))
    | lit(U"...") >> x3::attr(ast::Parameter::createVarArgParameter());

const auto parameter_list_def = -(parameter % lit(U","));

const auto function_proto_def
  = identifier > template_params > lit(U"(") > parameter_list > lit(U")")
    > ((lit(U"->") > type_name)
       | x3::attr(ast::BuiltinType{codegen::BuiltinTypeKind::void_}));

const auto function_decl_def
  = lit(U"declare") >> lit(U"func") > function_proto > lit(U";");

const auto function_def_def = is_public >> lit(U"func") > function_proto > stmt;

const auto type_def_def
  = lit(U"typedef") > identifier > lit(U"=") > type_name > lit(U";");

const auto import__def
  = lit(U"import") > lit(U"\"") > path > lit(U"\"") > lit(U";");

const auto name_space_def
  = lit(U"namespace") > identifier > lit(U"{") > top_level_list > lit(U"}");

const auto top_level_def = name_space | function_decl | function_def
                           | class_decl | class_def | union_def | type_def
                           | import_;

const auto top_level_with_attr_def = -attribute >> top_level_def;

const auto top_level_list_def = *top_level_with_attr;

BOOST_SPIRIT_DEFINE(name_space)
BOOST_SPIRIT_DEFINE(is_public)
BOOST_SPIRIT_DEFINE(template_params)
BOOST_SPIRIT_DEFINE(class_key)
BOOST_SPIRIT_DEFINE(union_key)
BOOST_SPIRIT_DEFINE(variable_def_without_init)
BOOST_SPIRIT_DEFINE(member_initializer)
BOOST_SPIRIT_DEFINE(member_initializer_list)
BOOST_SPIRIT_DEFINE(constructor)
BOOST_SPIRIT_DEFINE(destructor)
BOOST_SPIRIT_DEFINE(class_member_list)
BOOST_SPIRIT_DEFINE(class_def)
BOOST_SPIRIT_DEFINE(class_decl)
BOOST_SPIRIT_DEFINE(union_tag)
BOOST_SPIRIT_DEFINE(union_tag_list)
BOOST_SPIRIT_DEFINE(union_def)
BOOST_SPIRIT_DEFINE(parameter)
BOOST_SPIRIT_DEFINE(parameter_list)
BOOST_SPIRIT_DEFINE(function_proto)
BOOST_SPIRIT_DEFINE(function_decl)
BOOST_SPIRIT_DEFINE(function_def)
BOOST_SPIRIT_DEFINE(type_def)
BOOST_SPIRIT_DEFINE(import_)
BOOST_SPIRIT_DEFINE(top_level)
BOOST_SPIRIT_DEFINE(top_level_with_attr)
BOOST_SPIRIT_DEFINE(top_level_list)

//===----------------------------------------------------------------------===//
// Comment rules definition
//===----------------------------------------------------------------------===//

const auto single_line_comment_def
  = lit(U"//") >> *(char_ - x3::eol) >> (x3::eol | x3::eoi);

const auto block_comment_def
  = lit(U"/*") >> *(block_comment | (char_ - lit(U"*/"))) >> lit(U"*/");

const auto comment_def = single_line_comment | block_comment;

BOOST_SPIRIT_DEFINE(single_line_comment)
BOOST_SPIRIT_DEFINE(comment)
BOOST_SPIRIT_DEFINE(block_comment)

//===----------------------------------------------------------------------===//
// Skipper rules definition
//===----------------------------------------------------------------------===//

const auto skipper_def = x3::space | comment;

BOOST_SPIRIT_DEFINE(skipper)

//===----------------------------------------------------------------------===//
// Translation unit rule and tag definition
//===----------------------------------------------------------------------===//

const auto translation_unit_def = top_level_list > x3::eoi;

BOOST_SPIRIT_DEFINE(translation_unit)

#pragma clang diagnostic pop

} // namespace syntax

Parser::Parser(std::string&& input, const std::filesystem::path& file)
  : input{std::move(input)}
  , u32_first{this->input.cbegin()}
  , u32_last{this->input.cend()}
  , positions{u32_first, u32_last}
  , file{file}
{
  parse();
}

void Parser::parse()
{
  x3::error_handler<InputIterator> error_handler{u32_first,
                                                 u32_last,
                                                 std::cerr,
                                                 file.string()};

  const auto parser = x3::with<x3::error_handler_tag>(
    std::ref(error_handler))[x3::with<PositionCacheTag>(
    positions)[syntax::translation_unit]];

  if (!x3::phrase_parse(u32_first, u32_last, parser, syntax::skipper, ast)
      || u32_first != u32_last) {
    // Some error occurred in parsing.
    throw ParseError{"compilation terminated."};
  }
}

} // namespace twinkle::parse
