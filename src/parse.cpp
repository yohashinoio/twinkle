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
  x3::error_handler_result on_error(Iterator,
                                    Iterator,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    ++total_errors;

    auto&& error_handler = x3::get<x3::error_handler_tag>(context).get();
    error_handler(x.where(),
                  format_error_message_without_filename("expected: " + x.which()
                                                        + " here:"));
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

const auto char_to_string = [](auto&& ctx) -> void {
  x3::_val(ctx) = std::string{x3::_attr(ctx)};
};

} // namespace action

namespace peg
{

const x3::rule<struct expression_tag, ast::operand> expression{"expression"};
const x3::rule<struct equality_tag, ast::operand>   equality{"equality"};
const x3::rule<struct relational_tag, ast::operand> relational{"relational"};
const x3::rule<struct add_tag, ast::operand>        add{"add"};
const x3::rule<struct mul_tag, ast::operand>        mul{"mul"};
const x3::rule<struct unary_tag, ast::operand>      unary{"unary"};
const x3::rule<struct primary_tag, ast::operand>    primary{"primary"};
const x3::rule<struct parser_tag, ast::operand>     parser{"parser"};

const auto data_type = x3::rule<struct data_type, std::string>{"data type"}
= x3::string("i32");

const auto integer = x3::rule<struct integer, int>{"integral number"}
= x3::int_;

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

const auto expression_def = equality;

const auto equality_def
  = relational[action::assign_attr_to_val]
    >> *(equality_operator > relational)[action::assign_binop_to_val];

const auto relational_def
  = add[action::assign_attr_to_val]
    >> *(relational_operator > add)[action::assign_binop_to_val];

const auto add_def = mul[action::assign_attr_to_val]
                     >> *(additive_operator > mul)[action::assign_binop_to_val];

const auto mul_def
  = unary[action::assign_attr_to_val]
    >> *(multitive_operator > unary)[action::assign_binop_to_val];

const auto unary_def = (unary_operator > primary)[action::assign_unaryop_to_val]
                       | primary[action::assign_attr_to_val];

const auto primary_def
  = (x3::lit('(') > expression > x3::lit(')'))[action::assign_attr_to_val]
    | x3::expect[integer][action::assign_attr_to_val];

const auto parser_def
  = x3::skip(x3::space)[x3::eoi /*Empty program*/ | (expression > x3::eoi)];

BOOST_SPIRIT_DEFINE(expression,
                    equality,
                    relational,
                    add,
                    mul,
                    unary,
                    primary,
                    parser)

struct expression_tag : with_error_handling {
};
struct equality_tag : with_error_handling {
};
struct relational_tag : with_error_handling {
};
struct add_tag : with_error_handling {
};
struct mul_tag : with_error_handling {
};
struct unary_tag : with_error_handling {
};
struct primary_tag : with_error_handling {
};
struct parser_tag : with_error_handling {
};

} // namespace peg

parser::parser(std::string&& input, const std::filesystem::path& path)
  : input{std::move(input)}
  , path{path}
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
                                                 path.string()};
  const auto                       parser
    = x3::with<x3::error_handler_tag>(std::ref(error_handler))[peg::parser];

  ast::program result;
  const auto   success = x3::parse(first, last, parser, result);

  if (!success || first != last) {
    throw std::runtime_error{format(COLOR_WHITE
                                    "\n%zu errors generated." COLOR_DEFAULT,
                                    with_error_handling::total_errors)};
  }

  return result;
}

} // namespace miko::parse
