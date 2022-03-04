//
//  parse.cpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#include "pch.hpp"
#include "ast_adapted.hpp"
#include "utility.hpp"
#include "parse.hpp"

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace miko::parse
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

const auto assign_function_decl_to_val = [](auto&& ctx) -> void {
  x3::_val(ctx) = ast::function_decl{x3::_attr(ctx)};
};

const auto assign_function_def_to_val = [](auto&& ctx) -> void {
  x3::_val(ctx)
    = ast::function_def{fusion::at_c<0>(x3::_attr(ctx)) /* function decl */,
                        fusion::at_c<1>(x3::_attr(ctx)) /* body */};
};

const auto char_to_string = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::string{x3::_attr(ctx)};
};

} // namespace action

namespace peg
{

// common rules
const auto identifier = x3::rule<struct identifier, std::string>{"identifier"}
= x3::raw[x3::lexeme[(x3::alpha | x3::lit('_'))
                     >> *(x3::alnum | x3::lit('_'))]];

const auto data_type = x3::rule<struct data_type, std::string>{"data type"}
= x3::string("i32");

const auto integer = x3::rule<struct integer, int>{"integral number"}
= x3::int_;

// expreesion rules
const x3::rule<struct equality_tag, ast::expression>   equality{"equality"};
const x3::rule<struct relational_tag, ast::expression> relational{"relational"};
const x3::rule<struct addition_tag, ast::expression>   addition{"addition"};
const x3::rule<struct multiplication_tag, ast::expression> multiplication{
  "multiplication"};
const x3::rule<struct unary_tag, ast::expression>      unary{"unary"};
const x3::rule<struct primary_tag, ast::expression>    primary{"primary"};
const x3::rule<struct expression_tag, ast::expression> expression{"expression"};

const auto unary_operator
  = x3::rule<struct unary_operator, std::string>{"unary operator"}
= x3::char_("+-")[action::char_to_string];

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

const auto primary_def = (x3::lit('(') > expression > x3::lit(')')) | integer;

const auto expression_def = equality;

// statement rules
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
= expression_statement | return_statement;

const auto compound_statement
  = x3::rule<struct compound_statement_tag,
             ast::compound_statement>{"compound statement"}
= x3::lit('{') > *statement > x3::lit('}');

// function rules
const auto function_proto = x3::rule<struct function_proto_tag,
                                     ast::function_decl>{"function prototype"}
= identifier[action::assign_function_decl_to_val] > x3::lit('(') > x3::lit(')');

const auto function_decl = x3::rule<struct function_decl_tag,
                                    ast::function_decl>{"function declaration"}
= x3::lit("extern") > function_proto > x3::lit(';');

const auto function_defi
  = x3::rule<struct function_defi_tag, ast::function_def>{"function definition"}
= x3::lit("fn") > function_proto > compound_statement;

// top level rule
const auto toplevel = x3::rule<struct toplevel_tag, ast::toplevel>{"top level"}
= function_decl | function_defi;

// parser rule
const auto parser = x3::rule<struct parser_tag, ast::program>{"parser"}
= x3::skip(x3::space)
  [x3::eoi /*Empty program*/ | (x3::expect[toplevel] >> *toplevel > x3::eoi)];

BOOST_SPIRIT_DEFINE(expression,
                    equality,
                    relational,
                    addition,
                    multiplication,
                    unary,
                    primary)

// expression tags definition
struct expression_tag : with_error_handling {};
struct equality_tag : with_error_handling {};
struct relational_tag : with_error_handling {};
struct addition_tag : with_error_handling {};
struct multiplication_tag : with_error_handling {};
struct unary_tag : with_error_handling {};
struct primary_tag : with_error_handling {};

// statement
struct return_statement_tag : with_error_handling {};
struct expression_statement_tag : with_error_handling {};
struct statement_tag : with_error_handling {};
struct compound_statement_tag : with_error_handling {};

// function tags definition
struct function_proto_tag : with_error_handling {};
struct function_decl_tag : with_error_handling {};
struct function_defi_tag : with_error_handling {};

// top level tag definition
struct toplevel_tag : with_error_handling {};

// parser tag definition
struct parser_tag : with_error_handling {};

} // namespace peg

parser::parser(std::string&& input, const std::filesystem::path& source)
  : input{std::move(input)}
  , source{source}
{
}

auto parser::parse() -> ast::program
{
  using iterator_type = std::string::const_iterator;

  iterator_type       first = input.cbegin();
  const iterator_type last  = input.cend();

  x3::error_handler<iterator_type> error_handler{first,
                                                 last,
                                                 std::cerr,
                                                 source.string()};
  const auto                       parser
    = x3::with<x3::error_handler_tag>(std::ref(error_handler))[peg::parser];

  ast::program result;
  const auto   success = x3::parse(first, last, parser, result);

  if (!success || first != last) {
    throw std::runtime_error{
      format("%zu errors generated.\n", with_error_handling::total_errors)};
  }

  return result;
}

} // namespace miko::parse
