/**
 * ast.hpp
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

#include <pch/pch.hpp>
#include <parse/id.hpp>

namespace x3 = boost::spirit::x3;

//===----------------------------------------------------------------------===//
// Abstract syntax tree
//===----------------------------------------------------------------------===//

namespace miko::ast
{

struct nil {};

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree
//===----------------------------------------------------------------------===//

struct string_literal : x3::position_tagged {
  std::string str;

  explicit string_literal(const std::string& str);

  string_literal();
};

struct variable_expr : x3::position_tagged {
  std::string name;

  explicit variable_expr(const std::string& name);

  variable_expr();
};

struct unary_op_expr;
struct binary_op_expr;
struct function_call_expr;
struct cast_expr;

using expression = boost::variant<nil,
                                  std::uint32_t, /* unsigned integer */
                                  std::int32_t,  /* signed integer */
                                  string_literal,
                                  variable_expr,
                                  boost::recursive_wrapper<unary_op_expr>,
                                  boost::recursive_wrapper<binary_op_expr>,
                                  boost::recursive_wrapper<function_call_expr>,
                                  boost::recursive_wrapper<cast_expr>>;

struct unary_op_expr : x3::position_tagged {
  std::string op;
  expression  rhs;

  unary_op_expr(const std::string& op, const expression& rhs);

  unary_op_expr();
};

struct binary_op_expr : x3::position_tagged {
  expression  lhs;
  std::string op;
  expression  rhs;

  binary_op_expr(const expression&  lhs,
                 const std::string& op,
                 const expression&  rhs);

  binary_op_expr();
};

struct function_call_expr : x3::position_tagged {
  std::string             callee;
  std::vector<expression> args;

  function_call_expr(const std::string&             callee,
                     const std::vector<expression>& args);

  function_call_expr();
};

struct cast_expr : x3::position_tagged {
  expression    rhs;
  id::type_name as;

  cast_expr(const expression& rhs, const id::type_name as);

  cast_expr();
};

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree
//===----------------------------------------------------------------------===//

struct return_statement : x3::position_tagged {
  std::optional<expression> rhs;

  explicit return_statement(const std::optional<expression>& rhs);

  return_statement();
};

struct variable_def_statement : x3::position_tagged {
  std::optional<id::variable_qualifier> qualifier;
  std::string                           name;
  id::type_name                         type;
  std::optional<expression>             initializer;

  variable_def_statement(const std::optional<id::variable_qualifier>& qualifier,
                         const std::string&                           name,
                         const id::type_name                          type,
                         const std::optional<expression>& initializer);

  variable_def_statement();
};

struct if_statement;
struct for_statement;

using statement = boost::variant<nil,
                                 expression,
                                 return_statement,
                                 variable_def_statement,
                                 boost::recursive_wrapper<if_statement>,
                                 boost::recursive_wrapper<for_statement>>;

// expression or { *(expression ';') }
using compound_statement = std::vector<statement>;

struct if_statement : x3::position_tagged {
  expression                        condition;
  compound_statement                then_statement;
  std::optional<compound_statement> else_statement;

  if_statement(const expression&                        condition,
               const compound_statement&                then_statement,
               const std::optional<compound_statement>& else_statement);

  if_statement();
};

struct for_statement : x3::position_tagged {
  std::optional<expression> init_expression;
  std::optional<expression> cond_expression;
  std::optional<expression> loop_expression;
  compound_statement        body;

  for_statement(const std::optional<expression>& init_expression,
                const std::optional<expression>& cond_expression,
                const std::optional<expression>& loop_expression,
                const compound_statement&        body);

  for_statement();
};

//===----------------------------------------------------------------------===//
// Top level abstract syntax tree
//===----------------------------------------------------------------------===//

struct parameter : x3::position_tagged {
  std::optional<id::variable_qualifier> qualifier;
  std::string                           name;
  id::type_name                         type;

  parameter(const std::optional<id::variable_qualifier>& qualifier,
            const std::string&                           name,
            const id::type_name                          type);

  parameter();
};

struct function_declare : x3::position_tagged {
  std::optional<id::function_linkage> linkage;
  std::string                         name;
  std::vector<parameter>              params;
  id::type_name                       return_type;

  function_declare(const std::optional<id::function_linkage>& linkage,
                   const std::string&                         name,
                   const std::vector<parameter>&              params,
                   const id::type_name                        return_type);

  function_declare();
};

struct function_define : x3::position_tagged {
  function_declare   decl;
  compound_statement body;

  function_define(const function_declare& decl, const compound_statement& body);

  function_define();
};

using top_level_stmt = boost::variant<nil, function_declare, function_define>;

using program = std::vector<top_level_stmt>;

} // namespace miko::ast

#endif