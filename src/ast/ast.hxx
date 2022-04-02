/**
 * ast.hxx
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _1d3d3a84_9536_11ec_b909_0242ac120002
#define _1d3d3a84_9536_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hxx>
#include <parse/id.hxx>

namespace x3 = boost::spirit::x3;

namespace miko
{

//===----------------------------------------------------------------------===//
// Abstract syntax tree
//===----------------------------------------------------------------------===//

namespace ast
{

struct type_info {
  bool          is_ptr;
  id::type_name id;
};

struct nil {};

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree
//===----------------------------------------------------------------------===//

struct string_literal : x3::position_tagged {
  std::string str;
};

struct variable_ref : x3::position_tagged {
  std::string name;
};

struct unary_op_expr;
struct bin_op_expr;
struct function_call_expr;
struct conv_expr;
struct addr_of_expr;

using expression = boost::variant<nil,
                                  std::uint32_t, /* Unsigned integer literals */
                                  std::int32_t,  /* Signed integer literals */
                                  bool,          /* Boolean literals */
                                  string_literal,
                                  variable_ref,
                                  boost::recursive_wrapper<unary_op_expr>,
                                  boost::recursive_wrapper<bin_op_expr>,
                                  boost::recursive_wrapper<function_call_expr>,
                                  boost::recursive_wrapper<conv_expr>,
                                  boost::recursive_wrapper<addr_of_expr>>;

struct unary_op_expr : x3::position_tagged {
  std::string op;
  expression  rhs;
};

struct bin_op_expr : x3::position_tagged {
  expression  lhs;
  std::string op;
  expression  rhs;

  bin_op_expr(decltype(lhs)&& lhs, decltype(op)&& op, decltype(rhs)&& rhs)
    : lhs{std::move(lhs)}
    , op{std::move(op)}
    , rhs{std::move(rhs)}
  {
  }

  bin_op_expr() noexcept
  {
  }
};

struct function_call_expr : x3::position_tagged {
  std::string             callee;
  std::vector<expression> args;
};

struct conv_expr : x3::position_tagged {
  expression lhs;
  type_info  as;
};

struct addr_of_expr : x3::position_tagged {
  expression lhs;
};

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree
//===----------------------------------------------------------------------===//

struct return_statement : x3::position_tagged {
  std::optional<expression> rhs;
};

struct variable_def_statement : x3::position_tagged {
  std::optional<id::variable_qualifier> qualifier;
  std::string                           name;
  type_info                             type;
  std::optional<expression>             initializer;
};

struct break_statement : x3::position_tagged {
  std::string tmp;
};

struct continue_statement : x3::position_tagged {
  std::string tmp;
};

struct if_statement;
struct loop_statement;
struct while_statement;
struct for_statement;

using statement = boost::make_recursive_variant<
  nil,
  std::vector<boost::recursive_variant_>, // compound statement
  expression,
  return_statement,
  variable_def_statement,
  break_statement,
  continue_statement,
  boost::recursive_wrapper<if_statement>,
  boost::recursive_wrapper<loop_statement>,
  boost::recursive_wrapper<while_statement>,
  boost::recursive_wrapper<for_statement>>::type;

using compound_statement = std::vector<statement>;

struct if_statement : x3::position_tagged {
  expression               condition;
  statement                then_statement;
  std::optional<statement> else_statement;
};

struct loop_statement : x3::position_tagged {
  std::string tmp;
  statement   body;
};

struct while_statement : x3::position_tagged {
  expression cond_expr;
  statement  body;
};

struct for_statement : x3::position_tagged {
  std::optional<expression> init_expr;
  std::optional<expression> cond_expr;
  std::optional<expression> loop_expr;
  statement                 body;
};

//===----------------------------------------------------------------------===//
// Top level abstract syntax tree
//===----------------------------------------------------------------------===//

struct parameter : x3::position_tagged {
  std::optional<id::variable_qualifier> qualifier;
  std::string                           name;
  type_info                             type;
  bool                                  is_vararg;

  parameter(decltype(qualifier)&&     qualifier,
            decltype(name)&&          name,
            const decltype(type)&     type,
            const decltype(is_vararg) is_vararg)
    : qualifier{qualifier}
    , name{name}
    , type{type}
    , is_vararg{is_vararg}
  {
  }

  parameter() noexcept
  {
  }
};

struct parameter_list : x3::position_tagged {
  std::vector<parameter> params;

  const parameter& operator[](const std::size_t idx) const
  {
    return params.at(idx);
  }

  const std::vector<parameter>& operator*() const noexcept
  {
    return params;
  }

  std::size_t length() const noexcept
  {
    return params.size();
  }
};

struct function_declare : x3::position_tagged {
  std::optional<id::function_linkage> linkage;
  std::string                         name;
  parameter_list                      params;
  type_info                           return_type;
};

struct function_define : x3::position_tagged {
  function_declare decl;
  statement        body;
};

using top_level = boost::variant<nil, function_declare, function_define>;

using program = std::vector<top_level>;

} // namespace ast
} // namespace miko

#endif
