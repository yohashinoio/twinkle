/**
 * ast.cxx
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <ast/ast.hxx>

namespace x3 = boost::spirit::x3;

//===----------------------------------------------------------------------===//
// Abstract syntax tree
//===----------------------------------------------------------------------===//

namespace miko::ast
{

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree
//===----------------------------------------------------------------------===//

string_literal::string_literal(const std::string& str)
  : str{str}
{
}

string_literal::string_literal() noexcept
{
}

unary_op_expr::unary_op_expr(const std::string& op, const expression& rhs)
  : op{op}
  , rhs{rhs}
{
}

unary_op_expr::unary_op_expr() noexcept
{
}

binary_op_expr::binary_op_expr(const expression&  lhs,
                               const std::string& op,
                               const expression&  rhs)
  : lhs{lhs}
  , op{op}
  , rhs{rhs}
{
}

binary_op_expr::binary_op_expr() noexcept
{
}

variable_expr::variable_expr(const std::string& name)
  : name{name}
{
}

variable_expr::variable_expr() noexcept
{
}

function_call_expr::function_call_expr(const std::string&             callee,
                                       const std::vector<expression>& args)
  : callee{callee}
  , args{args}
{
}

function_call_expr::function_call_expr() noexcept
{
}

cast_expr::cast_expr(const expression& lhs, const ast::type_info& as)
  : lhs{lhs}
  , as{as}
{
}

cast_expr::cast_expr() noexcept
{
}

address_of_expr::address_of_expr(const expression& lhs)
  : lhs{lhs}
{
}

address_of_expr::address_of_expr() noexcept
{
}

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree
//===----------------------------------------------------------------------===//

return_statement::return_statement(const std::optional<expression>& rhs)
  : rhs{rhs}
{
}

return_statement::return_statement() noexcept
{
}

variable_def_statement::variable_def_statement(
  const std::optional<id::variable_qualifier>& qualifier,
  const std::string&                           name,
  const ast::type_info&                        type,
  const std::optional<expression>&             initializer)
  : qualifier{qualifier}
  , name{name}
  , type{type}
  , initializer{initializer}
{
}

variable_def_statement::variable_def_statement() noexcept
{
}

if_statement::if_statement(const expression&               condition,
                           const statement&                then_statement,
                           const std::optional<statement>& else_statement)
  : condition{condition}
  , then_statement{then_statement}
  , else_statement{else_statement}
{
}

if_statement::if_statement() noexcept
{
}

loop_statement::loop_statement(const std::string& tmp, const statement& body)
  : tmp{tmp}
  , body{body}
{
}

loop_statement::loop_statement() noexcept
{
}

while_statement::while_statement(const expression& cond_expr,
                                 const statement&  body)
  : cond_expr{cond_expr}
  , body{body}
{
}

while_statement::while_statement() noexcept
{
}

for_statement::for_statement(const std::optional<expression>& init_expr,
                             const std::optional<expression>& cond_expr,
                             const std::optional<expression>& loop_expr,
                             const statement&                 body)
  : init_expr{init_expr}
  , cond_expr{cond_expr}
  , loop_expr{loop_expr}
  , body{body}
{
}

for_statement::for_statement() noexcept
{
}

//===----------------------------------------------------------------------===//
// Top level statement abstract syntax tree
//===----------------------------------------------------------------------===//

parameter::parameter(const std::optional<id::variable_qualifier>& qualifier,
                     const std::string&                           name,
                     const ast::type_info&                        type,
                     bool                                         is_vararg)
  : qualifier{qualifier}
  , name{name}
  , type{type}
  , is_vararg{is_vararg}
{
}

parameter::parameter() noexcept
{
}

parameter_list::parameter_list(const std::vector<parameter>& params)
  : params{params}
{
}

parameter_list::parameter_list() noexcept
{
}

const parameter&
parameter_list::operator[](const std::size_t idx) const
{
  return params.at(idx);
}

const std::vector<parameter>& parameter_list::operator*() const noexcept
{
  return params;
}

std::size_t parameter_list::length() const noexcept
{
  return params.size();
}

function_declare::function_declare(
  const std::optional<id::function_linkage>& linkage,
  const std::string&                         name,
  const parameter_list&                      params,
  const ast::type_info&                      return_type)
  : linkage{linkage}
  , name{name}
  , params{params}
  , return_type{return_type}
{
}

function_declare::function_declare() noexcept
{
}

function_define::function_define(const function_declare& decl,
                                 const statement&        body)
  : decl{decl}
  , body{body}
{
}

function_define::function_define() noexcept
{
}

} // namespace miko::ast
