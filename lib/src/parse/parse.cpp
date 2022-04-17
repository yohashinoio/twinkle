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
#include <utils/type.hpp>
#include <utils/format.hpp>

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace maple::parse
{

//===----------------------------------------------------------------------===//
// Error handling
//===----------------------------------------------------------------------===//

struct ErrorHandle {
  template <typename Iterator, typename Context>
  x3::error_handler_result on_error([[maybe_unused]] Iterator&       first,
                                    [[maybe_unused]] const Iterator& last,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    ++total_errors;

    auto&& error_handler = x3::get<x3::error_handler_tag>(context).get();
    error_handler(x.where(),
                  formatErrorMessage(
                    "expected: " + boost::core::demangle(x.which().c_str())));

    return x3::error_handler_result::fail;
  }

  static std::size_t total_errors;
};

std::size_t ErrorHandle::total_errors = 0;

//===----------------------------------------------------------------------===//
// Annotations
//===----------------------------------------------------------------------===//

// Tag used to get the position cache from the context.
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

struct BuintinTypeSymbolsTag : x3::symbols<BuiltinTypeKind> {
  BuintinTypeSymbolsTag()
  {
    // clang-format off
    add
      ("void",   {BuiltinTypeKind::void_})
      (  "i8",      {BuiltinTypeKind::i8})
      (  "u8",      {BuiltinTypeKind::u8})
      ( "i16",     {BuiltinTypeKind::i16})
      ( "u16",     {BuiltinTypeKind::u16})
      ( "i32",     {BuiltinTypeKind::i32})
      ( "u32",     {BuiltinTypeKind::u32})
      ( "i64",     {BuiltinTypeKind::i64})
      ( "u64",     {BuiltinTypeKind::u64})
      ("bool",   {BuiltinTypeKind::bool_})
      ("char",   {BuiltinTypeKind::char_})
    ;
    // clang-format on
  }
} builtin_type_symbols;

struct VariableQualifierSymbolsTag : x3::symbols<VariableQual> {
  VariableQualifierSymbolsTag()
  {
    // clang-format off
    add
      ("mut", VariableQual::mutable_)
    ;
    // clang-format on
  }
} variable_qualifier_symbols;

struct FunctionLinkageSymbolsTag : x3::symbols<Linkage> {
  FunctionLinkageSymbolsTag()
  {
    // clang-format off
    add
      ("private", Linkage::internal)
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

const auto identifier_internal
  = x3::rule<struct IdentifierInternalTag, std::u32string>{"identifier"}
= x3::raw
  [x3::lexeme[(x3::unicode::graph - (x3::unicode::digit | x3::unicode::punct)
               | x3::lit('_'))
              >> *(x3::unicode::graph - x3::unicode::punct | x3::lit('_'))]];

const auto identifier
  = x3::rule<struct IdentifierTag, ast::Identifier>{"identifier"}
= identifier_internal;

const auto variable_qualifier
  = x3::rule<struct VariableQualifierTag, VariableQual>{"variable qualifier"}
= variable_qualifier_symbols;

const auto function_linkage
  = x3::rule<struct FunctionLinkageTag, Linkage>{"function linkage"}
= function_linkage_symbols;

const auto uint_32bit
  = x3::rule<struct UnsignedInteger32Tag, std::uint32_t>{"integral number"}
= x3::uint32;

const auto int_32bit
  = x3::rule<struct SignedInteger32Tag, std::int32_t>{"integral number"}
= x3::int32;

const auto uint_64bit = x3::rule<struct UnsignedInteger64Tag,
                                 std::uint64_t>{"integral number (64bit)"}
= x3::uint64;

const auto int_64bit
  = x3::rule<struct SignedInteger64Tag, std::int64_t>{"integral number (64bit)"}
= x3::int64;

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
  >> (x3::unicode::char_ - (x3::lit('\'') | x3::eol | x3::lit('\\'))
      | escape_char)
  > x3::lit('\'');

const auto type = x3::rule<struct TypeTag, std::shared_ptr<Type>>{"type"}
= (-x3::char_('*')
   >> builtin_type_symbols /* TODO: support double (recursion) ptr */
   >> -(x3::lit('[')
        >> x3::uint_ /* TODO: inference */ >> x3::lit(']')))[([](auto&& ctx) {
    if (fusion::at_c<0>(x3::_attr(ctx))) {
      // Pointer types.
      x3::_val(ctx) = std::make_shared<PointerType>(
        std::make_unique<BuiltinType>(fusion::at_c<1>(x3::_attr(ctx))));
      return;
    }

    if (fusion::at_c<2>(x3::_attr(ctx))) {
      // Array types.
      x3::_val(ctx) = std::make_shared<ArrayType>(
        std::make_unique<BuiltinType>(fusion::at_c<1>(x3::_attr(ctx))),
        *fusion::at_c<2>(x3::_attr(ctx)) /* Array size */);
      return;
    }

    // Fundamental (built-in) types.
    x3::_val(ctx)
      = std::make_shared<BuiltinType>(fusion::at_c<1>(x3::_attr(ctx)));
  })];

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
= x3::string("+") | x3::string("-") | x3::string("*")
  | x3::string("&"); // TODO: ! operator

//===----------------------------------------------------------------------===//
// Expression rules
//===----------------------------------------------------------------------===//

const x3::rule<struct ExprTag, ast::Expr>     expr{"expression"};
const x3::rule<struct EqualTag, ast::Expr>    equal{"equality operation"};
const x3::rule<struct RelationTag, ast::Expr> relation{"relational operation"};
const x3::rule<struct AddTag, ast::Expr>      add{"addition operation"};
const x3::rule<struct MulTag, ast::Expr>      mul{"multiplication operation"};
const x3::rule<struct UnaryTag, ast::Expr>    unary{"unary operation"};
const x3::rule<struct ConversionTag, ast::Expr> conversion{"conversion"};
const x3::rule<struct PrimaryTag, ast::Expr>    primary{"primary"};

// const x3::rule<struct SubscriptTag, ast::Indirection> subscript{"subscript"};

const x3::rule<struct ConversionInternalTag, ast::Conversion>
  conversion_internal{"conversion"};
const x3::rule<struct UnaryInternalTag, ast::UnaryOp> unary_internal{
  "unary operation"};

const x3::rule<struct ArgListTag, std::vector<ast::Expr>> arg_list{
  "argument list"};
const x3::rule<struct FunctionCallTag, ast::FunctionCall> function_call{
  "function call"};

const auto arg_list_def = -(expr % x3::lit(','));
const auto function_call_def
  = identifier >> x3::lit("(") > arg_list > x3::lit(")");

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
  = conversion[action::assign_attr_to_val]
    >> *(multitive_operator > conversion)[action::assign_binop_to_val];

const auto conversion_internal_def = unary >> x3::lit("as") > type;
const auto conversion_def          = conversion_internal | unary;

const auto unary_internal_def = unary_operator >> primary;
const auto unary_def          = unary_internal | primary;

const auto primary_def = int_32bit | uint_32bit | int_64bit | uint_64bit
                         | boolean_literal | string_literal | char_literal
                         | function_call | identifier
                         | (x3::lit('(') > expr > x3::lit(')'));

BOOST_SPIRIT_DEFINE(expr,
                    equal,
                    relation,
                    add,
                    mul,
                    conversion,
                    unary,
                    arg_list,
                    function_call,
                    conversion_internal,
                    unary_internal,
                    primary)

//===----------------------------------------------------------------------===//
// Statement rules
//===----------------------------------------------------------------------===//

const x3::rule<struct InitListTag, ast::InitList> init_list{"initializer list"};
const x3::rule<struct InitializerTag, ast::Initializer> initializer{
  "initializer"};
const x3::rule<struct ExprStmtTag, ast::Expr> expr_stmt{"expression statement"};
const x3::rule<struct VariableDefTag, ast::VariableDef> variable_def{
  "variable definition"};
const x3::rule<struct AssignTag, ast::Assignment> assignment{
  "assignment statement"};
const x3::rule<struct PrefixIncOrDec, ast::PrefixIncAndDec> prefix_inc_or_dec{
  "prefix increment or decrement"};
const x3::rule<struct ReturnTag, ast::Return> _return{"return statement"};
const x3::rule<struct IfTag, ast::If>         _if{"if else statement"};
const x3::rule<struct LoopTag, ast::Loop>     _loop{"loop statement"};
const x3::rule<struct WhileTag, ast::While>   _while{"while statement"};
const x3::rule<struct ForTag, ast::For>       _for{"for statement"};
const x3::rule<struct StmtTag, ast::Stmt>     stmt{"statement"};

const auto init_list_def = x3::lit('{') > (expr % x3::lit(',')) > x3::lit('}');

const auto initializer_def = expr | init_list;

const auto expr_stmt_def = expr;

const auto assignment_def = expr >> assignment_operator > expr;

const auto prefix_inc_or_dec_def = (x3::string("++") | x3::string("--")) > expr;

const auto variable_type
  = x3::rule<struct variable_type_tag, std::shared_ptr<Type>>{"variable type"}
= type - x3::lit("void");

const auto variable_def_def = x3::lit("let") > -variable_qualifier > identifier
                              > -(x3::lit(':') > variable_type)
                              > -(x3::lit('=') > initializer);

const auto _return_def = x3::lit("ret") > -expr;

const auto _if_def = x3::lit("if") > x3::lit('(') > expr > x3::lit(')') > stmt
                     > -(x3::lit("else") > stmt);

const auto _loop_def = x3::string("loop") > stmt;

const auto _while_def = x3::lit("while") > x3::lit('(') > expr /* Condition */
                        > x3::lit(')') > stmt;

const auto _for_def
  = x3::lit("for") > x3::lit('(') > -(assignment | variable_def) /* Init */
    > x3::lit(';') > -expr                                       /* Condition */
    > x3::lit(';') > -(prefix_inc_or_dec | assignment)           /* Loop */
    > x3::lit(')') > stmt;

const auto _break = x3::rule<struct BreakTag, ast::Break>{"break statement"}
= x3::string("break");

const auto _continue
  = x3::rule<struct ContinueTag, ast::Continue>{"continue statement"}
= x3::string("continue");

const auto stmt_def
  = x3::lit(';')                          /* Null statement */
    | x3::lit('{') > *stmt > x3::lit('}') /* Compound statement */
    | _loop | _while | _for | _if | _break > x3::lit(';')
    | _continue > x3::lit(';') | _return > x3::lit(';')
    | prefix_inc_or_dec > x3::lit(';') | assignment > x3::lit(';')
    | variable_def > x3::lit(';') | expr_stmt > x3::lit(';');

BOOST_SPIRIT_DEFINE(init_list,
                    initializer,
                    expr_stmt,
                    variable_def,
                    assignment,
                    prefix_inc_or_dec,
                    _return,
                    _if,
                    _loop,
                    _while,
                    _for,
                    stmt)

//===----------------------------------------------------------------------===//
// Top level rules
//===----------------------------------------------------------------------===//

using namespace std::literals::string_literals;

const auto parameter
  = x3::rule<struct ParameterTag, ast::Parameter>{"parameter"}
= (-variable_qualifier >> identifier > x3::lit(':') > type > x3::attr(false))
  | x3::lit("...")
      >> x3::attr(ast::Parameter{std::nullopt, ast::Identifier{}, {}, true});

const auto parameter_list
  = x3::rule<struct ParameterListTag, ast::ParameterList>{"parameter list"}
= -(parameter % x3::lit(','));

const auto function_proto
  = x3::rule<struct FunctionProtoTag, ast::FunctionDecl>{"function prototype"}
= type > -function_linkage > identifier > x3::lit('(') > parameter_list
  > x3::lit(')');

const auto function_decl
  = x3::rule<struct FunctionDeclTag, ast::FunctionDecl>{"function declaration"}
= x3::lit("extern") > function_proto > x3::lit(';');

const auto function_def
  = x3::rule<struct FunctionDefTag, ast::FunctionDef>{"function definition"}
= function_proto > stmt;

const auto top_level = x3::rule<struct TopLevelTag, ast::TopLevel>{"top level"}
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

struct IdentifierTag : AnnotatePosition {};

struct StringLiteralTag
  : ErrorHandle
  , AnnotatePosition {};

struct CharLiteralTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Expression tags
//===----------------------------------------------------------------------===//

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

struct ConversionTag
  : ErrorHandle
  , AnnotatePosition {};

struct UnaryTag
  : ErrorHandle
  , AnnotatePosition {};

struct ConversionInternalTag
  : ErrorHandle
  , AnnotatePosition {};

struct UnaryInternalTag
  : ErrorHandle
  , AnnotatePosition {};

// struct SubscriptTag
//   : ErrorHandle
//   , AnnotatePosition {}; // TODO

struct ArgListTag : ErrorHandle {};

struct FunctionCallTag
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

struct InitListTag
  : ErrorHandle
  , AnnotatePosition {};

struct InitializerTag
  : ErrorHandle
  , AnnotatePosition {};

struct ExprStmtTag
  : ErrorHandle
  , AnnotatePosition {};

struct VariableDefTag
  : ErrorHandle
  , AnnotatePosition {};

struct AssignTag
  : ErrorHandle
  , AnnotatePosition {};

struct PrefixIncOrDec
  : ErrorHandle
  , AnnotatePosition {};

struct PrefixDecrement
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

struct BreakTag
  : ErrorHandle
  , AnnotatePosition {};

struct ContinueTag
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

Parser::Parser(std::string&& input, std::filesystem::path&& file)
  : input{std::move(input)}
  , u32_first{boost::u8_to_u32_iterator<std::string::const_iterator>{
      this->input.cbegin()}}
  , u32_last{boost::u8_to_u32_iterator<std::string::const_iterator>{
      this->input.cend()}}
  , positions{u32_first, u32_last}
  , file{std::move(file)}
{
  parse();
}

void Parser::parse()
{
  x3::error_handler<InputIterator> error_handler{u32_first,
                                                 u32_last,
                                                 std::cerr,
                                                 file.string()};

  const auto parser = x3::with<x3::error_handler_tag>(std::ref(
    error_handler))[x3::with<PositionCacheTag>(positions)[syntax::program]];

  const auto success
    = x3::phrase_parse(u32_first, u32_last, parser, syntax::skipper, ast);

  if (!success || u32_first != u32_last) {
    throw std::runtime_error{
      format("%zu errors generated.\n", ErrorHandle::total_errors)};
  }
}

} // namespace maple::parse
