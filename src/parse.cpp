//
//  parse.cpp
//
//  Copyright (c) 2022 The Miko Authors.
//  MIT License
//

#include "pch.hpp"
#include "ast_adapted.hpp"
#include "parse.hpp"

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace miko
{

namespace parse
{

struct with_error_handling {
  template <typename Iterator, typename Context>
  x3::error_handler_result on_error([[maybe_unused]] Iterator&       first,
                                    [[maybe_unused]] const Iterator& last,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    ++total_errors;

    auto&& error_handler = x3::get<x3::error_handler_tag>(context).get();
    error_handler(
      x.where(),
      format_error_message_without_filename("expected: " + x.which()));
    return x3::error_handler_result::fail;
  }

  static std::size_t total_errors;
};

std::size_t with_error_handling::total_errors = 0;

struct position_cache_tag;
struct annotate_position {
  template <typename T, typename Iterator, typename Context>
  inline void on_success(const Iterator& first,
                         const Iterator& last,
                         T&              ast,
                         const Context&  context)
  {
    auto&& position_cache = x3::get<position_cache_tag>(context);
    position_cache.annotate(ast, first, last);
  }
};

namespace action
{

const auto assign_attr_to_val = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::move(x3::_attr(ctx));
};

const auto assign_unaryop_to_val = [](auto&& ctx) -> void {
  x3::_val(ctx)
    = ast::unaryop{fusion::at_c<0>(x3::_attr(ctx)) /* operator */,
                   fusion::at_c<1>(x3::_attr(ctx)) /* right hand side*/};
};

const auto assign_binop_to_val = [](auto&& ctx) -> void {
  x3::_val(ctx)
    = ast::binop{x3::_val(ctx),
                 fusion::at_c<0>(x3::_attr(ctx)) /* operator */,
                 fusion::at_c<1>(x3::_attr(ctx)) /* right hand side */};
};

const auto assign_variable_to_val = [](auto&& ctx) {
  x3::_val(ctx) = ast::variable{x3::_attr(ctx) /* name */};
};

const auto assign_function_call_to_val = [](auto&& ctx) {
  x3::_val(ctx)
    = ast::function_call{fusion::at_c<0>(x3::_attr(ctx)) /* callee */,
                         fusion::at_c<1>(x3::_attr(ctx)) /* arguments */};
};

const auto char_to_string = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::string{x3::_attr(ctx)};
};

} // namespace action

namespace peg
{

//////////////////
// common rules //
//////////////////
const auto identifier = x3::rule<struct identifier, std::string>{"identifier"}
= x3::raw[x3::lexeme[(x3::alpha | x3::lit('_'))
                     >> *(x3::alnum | x3::lit('_'))]];

const auto data_type = x3::rule<struct data_type, std::string>{"data type"}
= x3::string("i32");

const auto integer = x3::rule<struct integer, int>{"integral number"}
= x3::int_;

//////////////////////
// expreesion rules //
//////////////////////
const x3::rule<struct assignment_tag, ast::expression> assignment{"assignment"};
const x3::rule<struct equality_tag, ast::expression>   equality{"equality"};
const x3::rule<struct relational_tag, ast::expression> relational{"relational"};
const x3::rule<struct addition_tag, ast::expression>   addition{"addition"};
const x3::rule<struct multiplication_tag, ast::expression> multiplication{
  "multiplication"};
const x3::rule<struct unary_tag, ast::expression>      unary{"unary"};
const x3::rule<struct primary_tag, ast::expression>    primary{"primary"};
const x3::rule<struct expression_tag, ast::expression> expression{"expression"};

const auto assignment_operator
  = x3::rule<struct assignment_operator, std::string>{"assignment operator"}
= x3::string("=");

const auto equality_operator
  = x3::rule<struct equality_operator, std::string>{"equality operator"}
= x3::string("==") | x3::string("!=");

const auto relational_operator
  = x3::rule<struct relational_operator, std::string>{"relational operator"}
= x3::string("<=") | x3::string(">=") /* <= and >= must come first */
  | x3::string("<") | x3::string(">");

const auto additive_operator
  = x3::rule<struct additive_operator, std::string>{"additive operator"}
= x3::char_("+-")[action::char_to_string];

const auto multitive_operator
  = x3::rule<struct multitive_operator, std::string>{"multitive operator"}
= x3::char_("*/")[action::char_to_string];

const auto unary_operator
  = x3::rule<struct unary_operator, std::string>{"unary operator"}
= x3::char_("+-")[action::char_to_string];

const auto argument_list
  = x3::rule<struct argument_list_tag,
             std::vector<ast::expression>>{"argument list"}
= -(expression >> *(x3::lit(',') > expression));

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

const auto unary_def = (unary_operator > primary)[action::assign_unaryop_to_val]
                       | primary[action::assign_attr_to_val];

const auto primary_def
  = (x3::lit('(') > expression > x3::lit(')'))[action::assign_attr_to_val]
    | integer[action::assign_attr_to_val]
    | (identifier >> x3::lit("(") > argument_list
       > x3::lit(")"))[action::assign_function_call_to_val]
    | identifier[action::assign_variable_to_val];

/////////////////////
// statement rules //
/////////////////////
const auto variable_def_statement
  = x3::rule<struct variable_def_statement_tag,
             ast::variable_def>{"variable definition"}
= x3::lit("let") > identifier >> -(x3::lit('=') > expression) > x3::lit(';');

const auto return_statement
  = x3::rule<struct return_statement_tag,
             ast::return_statement>{"return statement"}
= x3::lit("ret") > expression > x3::lit(';');

const auto expression_statement
  = x3::rule<struct expression_statement_tag,
             ast::expression>{"expression statement"}
= expression > x3::lit(';');

const auto statement
  = x3::rule<struct statement_tag, ast::statement>{"statement"}
= return_statement /* If return_statement does not have a higher priority than
                      expression_statement, "ret" will match the identifier.*/
  | variable_def_statement /* If variable_def_statement does not have a higher
                       priority than expression_statement, "let" will match the
                       identifier.*/
  | expression_statement;

const auto compound_statement
  = x3::rule<struct compound_statement_tag,
             ast::compound_statement>{"compound statement"}
= x3::lit('{') > *statement > x3::lit('}');

////////////////////
// function rules //
////////////////////
const auto parameter_list = x3::rule<struct parameter_list_tag,
                                     std::vector<std::string>>{"parameter list"}
= -(identifier >> *(x3::lit(',') > identifier));

const auto function_proto = x3::rule<struct function_proto_tag,
                                     ast::function_decl>{"function prototype"}
= identifier > x3::lit('(') > parameter_list > x3::lit(')');

const auto function_decl = x3::rule<struct function_decl_tag,
                                    ast::function_decl>{"function declaration"}
= x3::lit("extern") > function_proto > x3::lit(';');

const auto function_defi
  = x3::rule<struct function_defi_tag, ast::function_def>{"function definition"}
= x3::lit("func") > function_proto > compound_statement;

/////////////////
// parser rule //
/////////////////
const auto program = x3::rule<struct program_tag, ast::program>{"program"}
= *(function_decl | function_defi) > x3::eoi;

BOOST_SPIRIT_DEFINE(assignment,
                    expression,
                    equality,
                    relational,
                    addition,
                    multiplication,
                    unary,
                    primary)

/////////////////////
// expression tags //
/////////////////////
struct argument_list_tag
  : with_error_handling
  , annotate_position {};
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
struct unary_tag
  : with_error_handling
  , annotate_position {};
struct primary_tag
  : with_error_handling
  , annotate_position {};

////////////////////
// statement tags //
////////////////////
struct variable_def_statement
  : with_error_handling
  , annotate_position {};
struct return_statement_tag
  : with_error_handling
  , annotate_position {};
struct expression_statement_tag
  : with_error_handling
  , annotate_position {};
struct statement_tag
  : with_error_handling
  , annotate_position {};
struct compound_statement_tag
  : with_error_handling
  , annotate_position {};

///////////////////
// function tags //
///////////////////
struct parameter_list_tag
  : with_error_handling
  , annotate_position {};
struct function_proto_tag
  : with_error_handling
  , annotate_position {};
struct function_decl_tag
  : with_error_handling
  , annotate_position {};
struct function_defi_tag
  : with_error_handling
  , annotate_position {};

/////////////////
// program tag //
/////////////////
struct program_tag
  : with_error_handling
  , annotate_position {};

} // namespace peg

parser::parser(input_iterator_type          first,
               const input_iterator_type    last,
               const std::filesystem::path& source)
  : first{first}
  , last{last}
  , positions{first, last}
  , source{source}
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
                                                       source.string()};

  const auto parser = x3::with<x3::error_handler_tag>(std::ref(
    error_handler))[x3::with<position_cache_tag>(positions)[peg::program]];

  const auto success = x3::phrase_parse(first, last, parser, x3::space, ast);

  if (!success || first != last) {
    throw std::runtime_error{
      format("%zu errors generated.\n", with_error_handling::total_errors)};
  }
}

} // namespace parse
} // namespace miko
