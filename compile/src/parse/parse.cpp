/**
 * parse.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <pch/pch.hpp>
#include <ast/ast_adapted.hpp>
#include <parse/parse.hpp>
#include <parse/id.hpp>
#include <utils/format.hpp>

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace miko::parse
{

//===----------------------------------------------------------------------===//
// Error handling
//===----------------------------------------------------------------------===//

// It's only false when testing.
static bool parsing_error_output = true;

struct ErrorHandle {
  template <typename Iterator, typename Context>
  x3::error_handler_result on_error([[maybe_unused]] Iterator&       first,
                                    [[maybe_unused]] const Iterator& last,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    ++total_errors;

    if (parsing_error_output) {
      auto&& error_handler = x3::get<x3::error_handler_tag>(context).get();
      error_handler(x.where(),
                    format_error_message_without_filename(
                      "expected: " + boost::core::demangle(x.which().c_str())));
    }

    return x3::error_handler_result::fail;
  }

  static std::size_t total_errors;
};

std::size_t ErrorHandle::total_errors = 0;

//===----------------------------------------------------------------------===//
// Annotations
//===----------------------------------------------------------------------===//

// tag used to get the position cache from the context
struct PositionCacheTag;

struct AnnotatePosition {
  template <typename T, typename Iterator, typename Context>
  inline void on_success(const Iterator& first,
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

const auto assign_attr_to_val = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::move(x3::_attr(ctx));
};

const auto assign_binop_to_val = [](auto&& ctx) -> void {
  ast::BinOp ast{std::move(x3::_val(ctx)),
                 std::move(fusion::at_c<0>(x3::_attr(ctx))),
                 std::move(fusion::at_c<1>(x3::_attr(ctx)))};

  auto&& position_cache = x3::get<PositionCacheTag>(ctx);
  position_cache.annotate(ast, x3::_where(ctx).begin(), x3::_where(ctx).end());

  x3::_val(ctx) = std::move(ast);
};

const auto char_to_string = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::string{x3::_attr(ctx)};
};

} // namespace action

//===----------------------------------------------------------------------===//
// Syntax
//===----------------------------------------------------------------------===//

namespace syntax
{

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-shift-op-parentheses"

//===----------------------------------------------------------------------===//
// Symbol table
//===----------------------------------------------------------------------===//

struct TypeNameSymbolsTag : x3::symbols<id::TypeName> {
  TypeNameSymbolsTag()
  {
    // clang-format off
    add
      ("void",   id::TypeName::void_)
      (  "i8",      id::TypeName::i8)
      (  "u8",      id::TypeName::u8)
      ( "i16",     id::TypeName::i16)
      ( "u16",     id::TypeName::u16)
      ( "i32",     id::TypeName::i32)
      ( "u32",     id::TypeName::u32)
      ( "i64",     id::TypeName::i64)
      ( "u64",     id::TypeName::u64)
      ("bool",   id::TypeName::bool_)
    ;
    // clang-format on
  }
} TypeNameSymbols;

struct VariableQualifierSymbolsTag : x3::symbols<id::VariableQualifier> {
  VariableQualifierSymbolsTag()
  {
    // clang-format off
    add
      ("mut", id::VariableQualifier::mutable_)
    ;
    // clang-format on
  }
} variable_qualifier_symbols;

struct FunctionLinkageSymbolsTag : x3::symbols<id::FunctionLinkage> {
  FunctionLinkageSymbolsTag()
  {
    // clang-format off
    add
      ("private", id::FunctionLinkage::private_)
    ;
    // clang-format on
  }
} function_linkage_symbols;

struct EscapeCharSymbolsTag : x3::symbols<char> {
  EscapeCharSymbolsTag()
  {
    // clang-format off
    add
      ("\\a", '\a')
      ("\\b", '\b')
      ("\\f", '\f')
      ("\\n", '\n')
      ("\\r", '\r')
      ("\\t", '\t')
      ("\\v", '\v')
      ("\\0", '\0')
      ("\\\\", '\\')
      ("\\\'", '\'')
      ("\\\"", '\"')
    ;
    // clang-format on
  }
} escape_char_symbols;

//===----------------------------------------------------------------------===//
// Common rules
//===----------------------------------------------------------------------===//

const auto identifier
  = x3::rule<struct IdentifierTag, std::string>{"identifier"}
= x3::raw[x3::lexeme[(x3::alpha | x3::lit('_'))
                     >> *(x3::alnum | x3::lit('_'))]];

const auto variable_ident
  = x3::rule<struct VariableIdentTag, ast::VariableRef>{"variable identifier"}
= identifier;

const auto type_name
  = x3::rule<struct TypeNameTag, ast::TypeInfo>{"pointer type name"}
= (-x3::char_('*')
   >> TypeNameSymbols /* TODO: support double (recursion) ptr */)[(
  [](auto&& ctx) {
    if (fusion::at_c<0>(x3::_attr(ctx))) {
      // Pointer.
      x3::_val(ctx) = ast::TypeInfo{true, fusion::at_c<1>(x3::_attr(ctx))};
    }
    else {
      // Not pointer.
      x3::_val(ctx) = ast::TypeInfo{false, fusion::at_c<1>(x3::_attr(ctx))};
    }
  })];

const auto variable_qualifier
  = x3::rule<struct VariableQualifierTag,
             id::VariableQualifier>{"variable qualifier"}
= variable_qualifier_symbols;

const auto function_linkage
  = x3::rule<struct FunctionLinkageTag, id::FunctionLinkage>{"function linkage"}
= function_linkage_symbols;

const auto unsigned_integer
  = x3::rule<struct UnsignedIntegerTag, std::uint32_t>{"integral number"}
= x3::uint32;

const auto signed_integer
  = x3::rule<struct SignedIntegerTag, std::int32_t>{"integral number"}
= x3::int32;

const auto boolean_literal
  = x3::rule<struct BooleanLiteralTag, bool>{"boolean literal"} = x3::bool_;

const auto escape_char
  = x3::rule<struct EscapeCharTag, unsigned char>{"escape character"}
= x3::lit("\\") >> x3::int_parser<char, 8, 1, 3>()     // Octal
  | x3::lit("\\x") >> x3::int_parser<char, 16, 2, 2>() // Hexadecimal
  | escape_char_symbols;

const auto string_literal
  = x3::rule<struct StringLiteralTag, ast::StringLiteral>{"string literal"}
= x3::lexeme[x3::lit('"')
             >> *(x3::char_ - (x3::lit('"') | x3::eol | x3::lit('\\'))
                  | escape_char)
             > x3::lit('"')];

const auto char_literal
  = x3::rule<struct CharLiteralTag, ast::CharLiteral>{"character literal"}
= x3::lit('\'')
  >> (x3::char_ - (x3::lit('\'') | x3::eol | x3::lit('\\')) | escape_char)
  > x3::lit('\'');

//===----------------------------------------------------------------------===//
// Operator rules
//===----------------------------------------------------------------------===//

const auto assignment_operator
  = x3::rule<struct AssignmentOperatorTag, std::string>{"assignment operator"}
= x3::string("=") | x3::string("+=") | x3::string("-=") | x3::string("*=")
  | x3::string("/=") | x3::string("%=");

const auto equality_operator
  = x3::rule<struct EqualityOperatorTag, std::string>{"equality operator"}
= x3::string("==") | x3::string("!=");

const auto relational_operator
  = x3::rule<struct RelationalOperatorTag, std::string>{"relational operator"}
= x3::string("<=") | x3::string(">=") /* <= and >= must come first */
  | x3::string("<") | x3::string(">");

const auto additive_operator
  = x3::rule<struct AdditiveOperatorTag, std::string>{"additive operator"}
= (x3::string("+") - x3::string("+=")) | (x3::string("-") - x3::string("-="));

const auto multitive_operator
  = x3::rule<struct MultitiveOperatorTag, std::string>{"multitive operator"}
= (x3::string("*") - x3::string("*=")) | (x3::string("/") - x3::string("/="))
  | (x3::string("%") - x3::string("%="));

const auto unary_operator
  = x3::rule<struct UnaryOperatorTag, std::string>{"unary operator"}
= x3::char_("+-")[action::char_to_string];

//===----------------------------------------------------------------------===//
// Expression rules
//===----------------------------------------------------------------------===//

const x3::rule<struct ExprTag, ast::Expr>     expr{"expression"};
const x3::rule<struct EqualTag, ast::Expr>    equal{"equality operation"};
const x3::rule<struct RelationTag, ast::Expr> relation{"relational operation"};
const x3::rule<struct AddTag, ast::Expr>      add{"addition operation"};
const x3::rule<struct MulTag, ast::Expr>      mul{"multiplication operation"};
const x3::rule<struct UnaryTag, ast::Expr>    unary{"unary operation"};
const x3::rule<struct PrimaryTag, ast::Expr>  primary{"primary"};

const x3::rule<struct ArgListTag, std::vector<ast::Expr>> arg_list{
  "argument list"};
const x3::rule<struct FunctionCallTag, ast::FunctionCall> function_call{
  "function call"};
const x3::rule<struct ConversionTag, ast::Conversion> conversion{"conversion"};
const x3::rule<struct AddressOfTag, ast::AddressOf>   address_of{"address-of"};
const x3::rule<struct IndirectionTag, ast::Indirection> indirection{
  "indirection"};

const auto expr_def = equal;

const auto equal_def
  = relation[action::assign_attr_to_val]
    >> *(equality_operator > relation)[action::assign_binop_to_val];

const auto relation_def
  = add[action::assign_attr_to_val]
    >> *(relational_operator > add)[action::assign_binop_to_val];

const auto add_def = mul[action::assign_attr_to_val]
                     >> *(additive_operator > mul)[action::assign_binop_to_val];

const auto mul_def
  = unary[action::assign_attr_to_val]
    >> *(multitive_operator > unary)[action::assign_binop_to_val];

const auto conversion_def = primary >> x3::lit("as") > type_name;

const auto address_of_def = x3::lit('&') > primary;

const auto indirection_def = x3::lit('*') > primary;

const auto unary_def = conversion | primary | (unary_operator > primary)
                       | address_of | indirection;

const auto arg_list_def = -(expr % x3::lit(','));

const auto function_call_def
  = identifier >> x3::lit("(") > arg_list > x3::lit(")");

const auto primary_def = unsigned_integer | signed_integer | boolean_literal
                         | string_literal | char_literal | function_call
                         | variable_ident
                         | (x3::lit('(') > expr > x3::lit(')'));

BOOST_SPIRIT_DEFINE(expr,
                    equal,
                    relation,
                    add,
                    mul,
                    unary,
                    primary,
                    arg_list,
                    function_call,
                    conversion,
                    address_of,
                    indirection)

//===----------------------------------------------------------------------===//
// Statement rules
//===----------------------------------------------------------------------===//

const x3::rule<struct ExprStmtTag, ast::Expr> expr_stmt{"expression statement"};
const x3::rule<struct VariableDefTag, ast::VariableDef> variable_def{
  "variable definition"};
const x3::rule<struct AssignTag, ast::Assignment> assignment{
  "assignment statement"};
const x3::rule<struct ReturnTag, ast::Return> _return{"return statement"};
const x3::rule<struct IfTag, ast::If>         _if{"if else statement"};
const x3::rule<struct LoopTag, ast::Loop>     _loop{"loop statement"};
const x3::rule<struct WhileTag, ast::While>   _while{"while statement"};
const x3::rule<struct ForTag, ast::For>       _for{"for statement"};
const x3::rule<struct StmtTag, ast::Stmt>     stmt{"statement"};

const auto expr_stmt_def = expr;

const auto assignment_def = expr >> assignment_operator > expr;

const auto variable_type
  = x3::rule<struct variable_type_tag, ast::TypeInfo>{"variable type"}
= type_name - x3::lit("void");

const auto variable_def_def = x3::lit("let") > -variable_qualifier > identifier
                              > x3::lit(':') > variable_type
                              > -(x3::lit('=') > expr);

const auto _return_def = x3::lit("ret") > -expr;

const auto _if_def = x3::lit("if") > x3::lit('(') > expr > x3::lit(')') > stmt
                     > -(x3::lit("else") > stmt);

const auto _loop_def = x3::string("loop") > stmt;

const auto _while_def = x3::lit("while") > x3::lit('(') > expr /* Condition */
                        > x3::lit(')') > stmt;

const auto _for_def
  = x3::lit("for") > x3::lit('(')
    > -(assignment | variable_def) /* Init */ /* TODO: support to statement */
    > x3::lit(';') > -expr                    /* Condition */
    > x3::lit(';') >> -assignment /* Loop */ > x3::lit(')') > stmt;

const auto _break = x3::rule<struct break_tag, ast::Break>{"break statement"}
= x3::string("break");

const auto _continue
  = x3::rule<struct continue_tag, ast::Continue>{"continue statement"}
= x3::string("continue");

const auto stmt_def
  = x3::lit(';')                          /* Null statement */
    | x3::lit('{') > *stmt > x3::lit('}') /* Compound statement */
    | _loop | _while | _for | _if | _break > x3::lit(';')
    | _continue > x3::lit(';') | _return > x3::lit(';')
    | assignment > x3::lit(';') | variable_def > x3::lit(';')
    | expr_stmt > x3::lit(';');

BOOST_SPIRIT_DEFINE(expr_stmt,
                    variable_def,
                    assignment,
                    _return,
                    _if,
                    _loop,
                    _while,
                    _for,
                    stmt)

//===----------------------------------------------------------------------===//
// Top level rules
//===----------------------------------------------------------------------===//

const auto parameter
  = x3::rule<struct ParameterTag, ast::Parameter>{"parameter"}
= (-variable_qualifier >> identifier > x3::lit(':') > type_name
   > x3::attr(false))
  | x3::lit("...") >> x3::attr(ast::Parameter{std::nullopt, "", {}, true});

const auto parameter_list
  = x3::rule<struct ParameterListTag, ast::ParameterList>{"parameter list"}
= -(parameter % x3::lit(','));

const auto function_proto
  = x3::rule<struct FunctionProtoTag, ast::FunctionDecl>{"function prototype"}
= -function_linkage > identifier > x3::lit('(') > parameter_list > x3::lit(')')
  > x3::lit("->") > type_name;

const auto function_decl
  = x3::rule<struct FunctionDeclTag, ast::FunctionDecl>{"function declaration"}
= x3::lit("extern") > function_proto > x3::lit(';');

const auto function_def
  = x3::rule<struct FunctionDefTag, ast::FunctionDef>{"function definition"}
= x3::lit("func") > function_proto > stmt;

const auto top_level
  = x3::rule<struct TopLevelTag, ast::TopLevel>{"top level statement"}
= function_decl | function_def;

//===----------------------------------------------------------------------===//
// Comment rules
//===----------------------------------------------------------------------===//

const x3::rule<struct BlockCommentTag> block_comment{"block comment"};

const auto single_line_comment
  = x3::rule<struct SingleLineCommenTag>{"single line comment"}
= x3::lit("//") >> *(x3::char_ - x3::eol) >> (x3::eol | x3::eoi);

const auto block_comment_def = x3::lit("/*")
                               >> *(block_comment | (x3::char_ - x3::lit("*/")))
                               >> x3::lit("*/");

const auto comment = x3::rule<struct CommentTag>{"comment"}
= single_line_comment | block_comment;

BOOST_SPIRIT_DEFINE(block_comment)

//===----------------------------------------------------------------------===//
// Skipper rule
//===----------------------------------------------------------------------===//

const auto skipper = x3::rule<struct SkipperTag>{"skipper"}
= x3::space | comment;

//===----------------------------------------------------------------------===//
// Program rule
//===----------------------------------------------------------------------===//

const auto program = x3::rule<struct ProgramTag, ast::Program>{"program"}
= *top_level > x3::eoi;

#pragma clang diagnostic pop

//===----------------------------------------------------------------------===//
// Common tags
//===----------------------------------------------------------------------===//

struct VariableIdentTag : AnnotatePosition {};

struct StringLiteralTag
  : ErrorHandle
  , AnnotatePosition {};

struct CharLiteralTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Expression tags
//===----------------------------------------------------------------------===//

struct AssignTag
  : ErrorHandle
  , AnnotatePosition {};

struct ExprTag
  : ErrorHandle
  , AnnotatePosition {};

struct EqualTag
  : ErrorHandle
  , AnnotatePosition {};

struct RelationTag
  : ErrorHandle
  , AnnotatePosition {};

struct AddTag
  : ErrorHandle
  , AnnotatePosition {};

struct MulTag
  : ErrorHandle
  , AnnotatePosition {};

struct ArgListTag : ErrorHandle {};

struct FunctionCallTag
  : ErrorHandle
  , AnnotatePosition {};

struct ConversionTag
  : ErrorHandle
  , AnnotatePosition {};

struct AddressOfTag
  : ErrorHandle
  , AnnotatePosition {};

struct IndirectionTag
  : ErrorHandle
  , AnnotatePosition {};

struct UnaryTag
  : ErrorHandle
  , AnnotatePosition {};

struct PrimaryTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Statement tags
//===----------------------------------------------------------------------===//

struct StmtTag
  : ErrorHandle
  , AnnotatePosition {};

struct ExprStmtTag
  : ErrorHandle
  , AnnotatePosition {};

struct VariableDefTag
  : ErrorHandle
  , AnnotatePosition {};

struct ReturnTag
  : ErrorHandle
  , AnnotatePosition {};

struct IfTag
  : ErrorHandle
  , AnnotatePosition {};

struct LoopTag
  : ErrorHandle
  , AnnotatePosition {};

struct WhileTag
  : ErrorHandle
  , AnnotatePosition {};

struct ForTag
  : ErrorHandle
  , AnnotatePosition {};

struct break_tag
  : ErrorHandle
  , AnnotatePosition {};

struct continue_tag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Top level statement tags
//===----------------------------------------------------------------------===//

struct ParameterTag
  : ErrorHandle
  , AnnotatePosition {};

struct ParameterListTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionProtoTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionDeclTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionDefTag
  : ErrorHandle
  , AnnotatePosition {};

struct TopLevelTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Program tag
//===----------------------------------------------------------------------===//

struct ProgramTag
  : ErrorHandle
  , AnnotatePosition {};

} // namespace syntax

Parser::Parser(std::string&&                input,
               const std::filesystem::path& file_path,
               const bool                   error_output)
  : input{std::move(input)}
  , first{this->input.cbegin()}
  , last{this->input.cend()}
  , positions{first, last}
  , file_path{file_path}
{
  parsing_error_output = error_output;

  parse();
}

Parser::Parser(const std::string&           input,
               const std::filesystem::path& file_path,
               const bool                   error_output)
  : input{input}
  , first{input.cbegin()}
  , last{input.cend()}
  , positions{first, last}
  , file_path{file_path}
{
  parsing_error_output = error_output;

  parse();
}

[[nodiscard]] const ast::Program& Parser::get_ast() const noexcept
{
  return ast;
}

[[nodiscard]] const PositionCache& Parser::get_positions() const noexcept
{
  return positions;
}

void Parser::parse()
{
  x3::error_handler<InputIterator> error_handler{first,
                                                 last,
                                                 std::cerr,
                                                 file_path.string()};

  const auto parser = x3::with<x3::error_handler_tag>(std::ref(
    error_handler))[x3::with<PositionCacheTag>(positions)[syntax::program]];

  const auto success
    = x3::phrase_parse(first, last, parser, syntax::skipper, ast);

  if (!success || first != last) {
    throw std::runtime_error{
      format("%zu errors generated.\n", ErrorHandle::total_errors)};
  }
}

} // namespace miko::parse
