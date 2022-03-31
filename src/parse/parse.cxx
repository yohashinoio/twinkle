/**
 * parse.cxx
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <pch/pch.hxx>
#include <ast/ast_adapted.hxx>
#include <parse/parse.hxx>
#include <parse/id.hxx>
#include <utils/format.hxx>

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace miko::parse
{

//===----------------------------------------------------------------------===//
// Error handling
//===----------------------------------------------------------------------===//

struct with_error_handling {
  template <typename Iterator, typename Context>
  x3::error_handler_result on_error([[maybe_unused]] Iterator&       first,
                                    [[maybe_unused]] const Iterator& last,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    ++total_errors;

    auto&& error_handler = x3::get<x3::error_handler_tag>(context).get();
    error_handler(x.where(),
                  format_error_message_without_filename(
                    "expected: " + boost::core::demangle(x.which().c_str())));
    return x3::error_handler_result::fail;
  }

  static std::size_t total_errors;
};

std::size_t with_error_handling::total_errors = 0;

//===----------------------------------------------------------------------===//
// Annotations
//===----------------------------------------------------------------------===//

// tag used to get the position cache from the context
struct position_cache_tag;

struct annotate_position {
  template <typename T, typename Iterator, typename Context>
  inline void on_success(const Iterator& first,
                         const Iterator& last,
                         T&              ast,
                         const Context&  ctx)
  {
    auto&& position_cache = x3::get<position_cache_tag>(ctx);
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
  ast::binary_op_expr ast{x3::_val(ctx),
                          fusion::at_c<0>(x3::_attr(ctx)),
                          fusion::at_c<1>(x3::_attr(ctx))};

  auto&& position_cache = x3::get<position_cache_tag>(ctx);
  position_cache.annotate(ast, x3::_where(ctx).begin(), x3::_where(ctx).end());

  x3::_val(ctx) = std::move(ast);
};

const auto char_to_string = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::string{x3::_attr(ctx)};
};

} // namespace action

//===----------------------------------------------------------------------===//
// Parsing expression grammar
//===----------------------------------------------------------------------===//

namespace peg
{

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-shift-op-parentheses"

//===----------------------------------------------------------------------===//
// Symbol table
//===----------------------------------------------------------------------===//

struct type_name_symbols_tag : x3::symbols<id::type_name> {
  type_name_symbols_tag()
  {
    // clang-format off
    add
      ("void",   id::type_name::void_)
      (  "i8",      id::type_name::i8)
      (  "u8",      id::type_name::u8)
      ( "i16",     id::type_name::i16)
      ( "u16",     id::type_name::u16)
      ( "i32",     id::type_name::i32)
      ( "u32",     id::type_name::u32)
      ( "i64",     id::type_name::i64)
      ( "u64",     id::type_name::u64)
      ("bool",   id::type_name::bool_)
    ;
    // clang-format on
  }
} type_name_symbols;

struct variable_qualifier_symbols_tag : x3::symbols<id::variable_qualifier> {
  variable_qualifier_symbols_tag()
  {
    // clang-format off
    add
      ("mut", id::variable_qualifier::mutable_)
    ;
    // clang-format on
  }
} variable_qualifier_symbols;

struct function_linkage_symbols_tag : x3::symbols<id::function_linkage> {
  function_linkage_symbols_tag()
  {
    // clang-format off
    add
      ("private", id::function_linkage::private_)
    ;
    // clang-format on
  }
} function_linkage_symbols;

struct escape_character_symbols_tag : x3::symbols<char> {
  escape_character_symbols_tag()
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
} escape_character_symbols;

//===----------------------------------------------------------------------===//
// Common rules
//===----------------------------------------------------------------------===//

const auto identifier
  = x3::rule<struct identifier_tag, std::string>{"identifier"}
= x3::raw[x3::lexeme[(x3::alpha | x3::lit('_'))
                     >> *(x3::alnum | x3::lit('_'))]];

const auto variable_identifier
  = x3::rule<struct variable_identifier_tag,
             ast::variable_expr>{"variable identifier"}
= identifier;

const auto type_name
  = x3::rule<struct type_name_tag, ast::type_info>{"pointer type name"}
= (-x3::char_('*') >> type_name_symbols)[([](auto&& ctx) {
    if (fusion::at_c<0>(x3::_attr(ctx))) {
      // pointer
      x3::_val(ctx) = ast::type_info{true, fusion::at_c<1>(x3::_attr(ctx))};
    }
    else {
      // not pointer
      x3::_val(ctx) = ast::type_info{false, fusion::at_c<1>(x3::_attr(ctx))};
    }
  })];

const auto variable_qualifier
  = x3::rule<struct variable_qualifier_tag,
             id::variable_qualifier>{"variable qualifier"}
= variable_qualifier_symbols;

const auto function_linkage = x3::rule<struct function_linkage_tag,
                                       id::function_linkage>{"function linkage"}
= function_linkage_symbols;

const auto unsigned_integer
  = x3::rule<struct unsigned_integer_tag, std::uint32_t>{"integral number"}
= x3::uint32;

const auto signed_integer
  = x3::rule<struct signed_integer_tag, std::int32_t>{"integral number"}
= x3::int32;

const auto boolean_literal
  = x3::rule<struct boolean_literal_tag, bool>{"boolean literal"} = x3::bool_;

const auto escape_character
  = x3::rule<struct escape_character_tag, unsigned char>{"escape character"}
= x3::lit("\\") >> x3::int_parser<char, 8, 1, 3>()     // Octal
  | x3::lit("\\x") >> x3::int_parser<char, 16, 2, 2>() // Hexadecimal
  | escape_character_symbols;

const auto string_literal
  = x3::rule<struct string_literal_tag, ast::string_literal>{"string literal"}
= x3::lexeme[x3::lit('"')
             > *(x3::char_ - (x3::lit('"') | x3::eol | x3::lit('\\'))
                 | escape_character)
             > x3::lit('"')];

//===----------------------------------------------------------------------===//
// Operator rules
//===----------------------------------------------------------------------===//

const auto assignment_operator
  = x3::rule<struct assignment_operator_tag, std::string>{"assignment operator"}
= x3::string("=") | x3::string("+=") | x3::string("-=") | x3::string("*=")
  | x3::string("/=") | x3::string("%=");

const auto equality_operator
  = x3::rule<struct equality_operator_tag, std::string>{"equality operator"}
= x3::string("==") | x3::string("!=");

const auto relational_operator
  = x3::rule<struct relational_operator_tag, std::string>{"relational operator"}
= x3::string("<=") | x3::string(">=") /* <= and >= must come first */
  | x3::string("<") | x3::string(">");

const auto additive_operator
  = x3::rule<struct additive_operator_tag, std::string>{"additive operator"}
= (x3::string("+") - x3::string("+=")) | (x3::string("-") - x3::string("-="));

const auto multitive_operator
  = x3::rule<struct multitive_operator_tag, std::string>{"multitive operator"}
= (x3::string("*") - x3::string("*=")) | (x3::string("/") - x3::string("/="))
  | (x3::string("%") - x3::string("%="));

const auto unary_operator
  = x3::rule<struct unary_operator_tag, std::string>{"unary operator"}
= x3::char_("+-")[action::char_to_string];

//===----------------------------------------------------------------------===//
// Expression rules
//===----------------------------------------------------------------------===//

const x3::rule<struct expression_tag, ast::expression> expression{"expression"};
const x3::rule<struct assignment_tag, ast::expression> assignment{
  "assignment operation"};
const x3::rule<struct equality_tag, ast::expression> equality{
  "equality operation"};
const x3::rule<struct relational_tag, ast::expression> relational{
  "relational operation"};
const x3::rule<struct addition_tag, ast::expression> addition{
  "addition operation"};
const x3::rule<struct multiplication_tag, ast::expression> multiplication{
  "multiplication operation"};
const x3::rule<struct unary_tag, ast::expression>   unary{"unary operation"};
const x3::rule<struct primary_tag, ast::expression> primary{"primary"};

const x3::rule<struct argument_list_tag, std::vector<ast::expression>>
  argument_list{"argument list"};
const x3::rule<struct function_call_operation_tag, ast::function_call_expr>
  function_call_operation{"function call operation"};
const x3::rule<struct cast_operation_tag, ast::cast_expr> cast_operation{
  "conversion operation"};
const x3::rule<struct address_of_operation, ast::address_of_expr>
  address_of_operation{"address-of operation"};

const auto argument_list_def = -(expression >> *(x3::lit(',') > expression));

const auto function_call_operation_def
  = identifier >> x3::lit("(") > argument_list > x3::lit(")");

const auto cast_operation_def = primary >> x3::lit("as") > type_name;

const auto address_of_operation_def = x3::lit('&') > expression;

const auto expression_def = assignment;

const auto assignment_def
  = equality[action::assign_attr_to_val]
    >> *(assignment_operator > equality)[action::assign_binop_to_val];

const auto equality_def
  = relational[action::assign_attr_to_val]
    >> *(equality_operator > relational)[action::assign_binop_to_val];

const auto relational_def
  = addition[action::assign_attr_to_val]
    >> *(relational_operator > addition)[action::assign_binop_to_val];

const auto addition_def
  = multiplication[action::assign_attr_to_val]
    >> *(additive_operator > multiplication)[action::assign_binop_to_val];

const auto multiplication_def
  = unary[action::assign_attr_to_val]
    >> *(multitive_operator > unary)[action::assign_binop_to_val];

const auto unary_def = cast_operation
                       | (unary_operator > primary) /* unary operation */
                       | function_call_operation | primary;

const auto primary_def = (x3::lit('(') > expression > x3::lit(')'))
                         | unsigned_integer | signed_integer | boolean_literal
                         | string_literal | address_of_operation
                         | variable_identifier;

BOOST_SPIRIT_DEFINE(expression,
                    assignment,
                    equality,
                    relational,
                    addition,
                    multiplication,
                    unary,
                    primary,
                    argument_list,
                    function_call_operation,
                    cast_operation,
                    address_of_operation)

//===----------------------------------------------------------------------===//
// Statement rules
//===----------------------------------------------------------------------===//

const x3::rule<struct expression_statement_tag, ast::expression>
  expression_statement{"expression statement"};
const x3::rule<struct variable_def_statement_tag, ast::variable_def_statement>
  variable_def_statement{"variable definition"};
const x3::rule<struct return_statement_tag, ast::return_statement>
  return_statement{"return statement"};
const x3::rule<struct if_statement_tag, ast::if_statement> if_statement{
  "if else statement"};
const x3::rule<struct loop_statement_tag, ast::loop_statement> loop_statement{
  "loop statement"};
const x3::rule<struct while_statement_tag, ast::while_statement>
  while_statement{"while statement"};
const x3::rule<struct for_statement_tag, ast::for_statement> for_statement{
  "for statement"};
const x3::rule<struct statement_tag, ast::statement> statement{"statement"};

const auto expression_statement_def = expression > x3::lit(';');

const auto variable_type
  = x3::rule<struct variable_type_tag, ast::type_info>{"variable type"}
= type_name - x3::lit("void");

const auto variable_def_statement_def
  = x3::lit("let") > -variable_qualifier > identifier > x3::lit(':')
    > variable_type > -(x3::lit('=') > expression) > x3::lit(';');

const auto return_statement_def = x3::lit("ret") > -expression > x3::lit(';');

const auto if_statement_def = x3::lit("if") > x3::lit('(') > expression
                              > x3::lit(')') > statement
                              > -(x3::lit("else") > statement);

const auto loop_statement_def = x3::string("loop") > statement;

const auto while_statement_def = x3::lit("while") > x3::lit('(')
                                 > expression /* condition */
                                 > x3::lit(')') > statement;

const auto for_statement_def
  = x3::lit("for") > x3::lit('(')
    > -expression /* TODO: support to statement */ > x3::lit(';')
    > -expression /* condition */
    > x3::lit(';') >> -expression /* loop */ > x3::lit(')') > statement;

const auto break_statement = x3::rule<struct break_statement_tag,
                                      ast::break_statement>{"break statement"}
= x3::string("break") > x3::lit(';');

const auto continue_statement
  = x3::rule<struct continue_statement_tag,
             ast::continue_statement>{"continue statement"}
= x3::string("continue") > x3::lit(';');

const auto statement_def
  = x3::lit(';')                               /* null statement */
    | x3::lit('{') > *statement > x3::lit('}') /* compound statement (block) */
    | loop_statement_def | while_statement_def | for_statement | break_statement
    | continue_statement | return_statement | variable_def_statement
    | if_statement | expression_statement;

BOOST_SPIRIT_DEFINE(expression_statement,
                    variable_def_statement,
                    return_statement,
                    if_statement,
                    loop_statement,
                    while_statement,
                    for_statement,
                    statement)

//===----------------------------------------------------------------------===//
// Top level rules
//===----------------------------------------------------------------------===//

const auto parameter
  = x3::rule<struct parameter_tag, ast::parameter>{"parameter"}
= -variable_qualifier >> identifier > x3::lit(':') > type_name;

const auto parameter_list
  = x3::rule<struct parameter_list_tag,
             std::vector<ast::parameter>>{"parameter list"}
= -(parameter > *(x3::lit(',') > parameter));

const auto function_proto
  = x3::rule<struct function_proto_tag,
             ast::function_declare>{"function prototype"}
= -function_linkage > identifier > x3::lit('(') > parameter_list > x3::lit(')')
  > x3::lit("->") > type_name;

const auto function_declare
  = x3::rule<struct function_declare_tag,
             ast::function_declare>{"function declaration"}
= x3::lit("extern") > function_proto > x3::lit(';');

const auto function_define
  = x3::rule<struct function_define_tag,
             ast::function_define>{"function definition"}
= x3::lit("func") > function_proto > statement;

const auto top_level_stmt = x3::rule<struct top_level_stmt_tag,
                                     ast::top_level_stmt>{"top level statement"}
= function_declare | function_define;

//===----------------------------------------------------------------------===//
// Comment rules
//===----------------------------------------------------------------------===//

const x3::rule<struct block_comment_tag> block_comment{"block comment"};

const auto single_line_comment
  = x3::rule<struct single_line_comment_tag>{"single line comment"}
= x3::lit("//") >> *(x3::char_ - x3::eol) >> (x3::eol | x3::eoi);

const auto block_comment_def = x3::lit("/*")
                               >> *(block_comment | (x3::char_ - x3::lit("*/")))
                               >> x3::lit("*/");

const auto comment = x3::rule<struct comment_tag>{"comment"}
= single_line_comment | block_comment;

BOOST_SPIRIT_DEFINE(block_comment)

//===----------------------------------------------------------------------===//
// Skipper rule
//===----------------------------------------------------------------------===//

const auto skipper = x3::rule<struct skipper_tag>{"skipper"}
= x3::space | comment;

//===----------------------------------------------------------------------===//
// Program rule
//===----------------------------------------------------------------------===//

const auto program = x3::rule<struct program_tag, ast::program>{"program"}
= *top_level_stmt > x3::eoi;

#pragma clang diagnostic pop

//===----------------------------------------------------------------------===//
// Common tags
//===----------------------------------------------------------------------===//

struct variable_identifier_tag : annotate_position {};

struct string_literal_tag
  : with_error_handling
  , annotate_position {};

//===----------------------------------------------------------------------===//
// Expression tags
//===----------------------------------------------------------------------===//

struct assignment_tag
  : with_error_handling
  , annotate_position {};

struct expression_tag
  : with_error_handling
  , annotate_position {};

struct equality_tag
  : with_error_handling
  , annotate_position {};

struct relational_tag
  : with_error_handling
  , annotate_position {};

struct addition_tag
  : with_error_handling
  , annotate_position {};

struct multiplication_tag
  : with_error_handling
  , annotate_position {};

struct argument_list_tag : with_error_handling {};

struct function_call_operation_tag
  : with_error_handling
  , annotate_position {};

struct cast_operation_tag
  : with_error_handling
  , annotate_position {};

struct address_of_operation
  : with_error_handling
  , annotate_position {};

struct unary_tag
  : with_error_handling
  , annotate_position {};

struct primary_tag
  : with_error_handling
  , annotate_position {};

//===----------------------------------------------------------------------===//
// Statement tags
//===----------------------------------------------------------------------===//

struct statement_tag
  : with_error_handling
  , annotate_position {};

struct expression_statement_tag
  : with_error_handling
  , annotate_position {};

struct variable_def_statement_tag
  : with_error_handling
  , annotate_position {};

struct return_statement_tag
  : with_error_handling
  , annotate_position {};

struct if_statement_tag
  : with_error_handling
  , annotate_position {};

struct loop_statement_tag
  : with_error_handling
  , annotate_position {};

struct while_statement_tag
  : with_error_handling
  , annotate_position {};

struct for_statement_tag
  : with_error_handling
  , annotate_position {};

struct break_statement_tag
  : with_error_handling
  , annotate_position {};

struct continue_statement_tag
  : with_error_handling
  , annotate_position {};

//===----------------------------------------------------------------------===//
// Top level statement tags
//===----------------------------------------------------------------------===//

struct parameter_tag
  : with_error_handling
  , annotate_position {};

struct parameter_list_tag
  : with_error_handling
  , annotate_position {};

struct function_proto_tag
  : with_error_handling
  , annotate_position {};

struct function_declare_tag
  : with_error_handling
  , annotate_position {};

struct function_define_tag
  : with_error_handling
  , annotate_position {};

struct top_level_stmt_tag
  : with_error_handling
  , annotate_position {};

//===----------------------------------------------------------------------===//
// Program tag
//===----------------------------------------------------------------------===//

struct program_tag
  : with_error_handling
  , annotate_position {};

} // namespace peg

parser::parser(std::string&& input, const std::filesystem::path& file_path)
  : input{std::move(input)}
  , first{this->input.cbegin()}
  , last{this->input.cend()}
  , positions{first, last}
  , file_path{file_path}
{
  parse();
}

parser::parser(const std::string& input, const std::filesystem::path& file_path)
  : input{input}
  , first{input.cbegin()}
  , last{input.cend()}
  , positions{first, last}
  , file_path{file_path}
{
  parse();
}

[[nodiscard]] const ast::program& parser::get_ast() const noexcept
{
  return ast;
}

[[nodiscard]] const position_cache& parser::get_positions() const noexcept
{
  return positions;
}

void parser::parse()
{
  x3::error_handler<input_iterator_type> error_handler{first,
                                                       last,
                                                       std::cerr,
                                                       file_path.string()};

  const auto parser = x3::with<x3::error_handler_tag>(std::ref(
    error_handler))[x3::with<position_cache_tag>(positions)[peg::program]];

  const auto success = x3::phrase_parse(first, last, parser, peg::skipper, ast);

  if (!success || first != last) {
    throw std::runtime_error{
      format("%zu errors generated.\n", with_error_handling::total_errors)};
  }
}

} // namespace miko::parse
