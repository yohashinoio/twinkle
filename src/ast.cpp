//
//  ast.cpp
//
//  Copyright (c) 2022 The Miko Authors.
//  Apache License v2.0
//

#include "ast.hpp"

namespace x3 = boost::spirit::x3;

//===----------------------------------------------------------------------===//
// Abstract syntax tree
//===----------------------------------------------------------------------===//

namespace miko::ast
{

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree
//===----------------------------------------------------------------------===//

unary_op_expr::unary_op_expr(const std::string& op, const expression& rhs)
  : op{op}
  , rhs{rhs}
{
}

unary_op_expr::unary_op_expr()
  : op{}
  , rhs{}
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

binary_op_expr::binary_op_expr()
  : lhs{}
  , op{}
  , rhs{}
{
}

variable_expr::variable_expr(const std::string& name)
  : name{name}
{
}

variable_expr::variable_expr()
  : name{}
{
}

function_call_expr::function_call_expr(const std::string&             callee,
                                       const std::vector<expression>& args)
  : callee{callee}
  , args{args}
{
}

function_call_expr::function_call_expr()
  : callee{}
  , args{}
{
}

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree
//===----------------------------------------------------------------------===//

variable_def_statement::variable_def_statement(
  const std::string&               name,
  const std::optional<expression>& initializer)
  : name{name}
  , initializer{initializer}
{
}

variable_def_statement::variable_def_statement()
  : name{}
  , initializer{}
{
}

return_statement::return_statement(const expression& rhs)
  : rhs{rhs}
{
}

return_statement::return_statement()
  : rhs{}
{
}

if_statement::if_statement(
  const expression&                        condition,
  const compound_statement&                then_statement,
  const std::optional<compound_statement>& else_statement)
  : condition{condition}
  , then_statement{then_statement}
  , else_statement{else_statement}
{
}

if_statement::if_statement()
  : condition{}
  , then_statement{}
  , else_statement{}
{
}

for_statement::for_statement(const expression&         init_expression,
                             const expression&         cond_expression,
                             const expression&         loop_expression,
                             const compound_statement& body)
  : init_expression{init_expression}
  , cond_expression{cond_expression}
  , loop_expression{loop_expression}
  , body{body}
{
}

for_statement::for_statement()
  : init_expression{}
  , cond_expression{}
  , loop_expression{}
  , body{}
{
}

//===----------------------------------------------------------------------===//
// Program abstract syntax tree
//===----------------------------------------------------------------------===//

function_declare::function_declare(const std::string&              name,
                                   const std::vector<std::string>& args)
  : name{name}
  , args{args}
{
}

function_declare::function_declare()
  : name{}
  , args{}
{
}

function_define::function_define(const function_declare&   decl,
                                 const compound_statement& body)
  : decl{decl}
  , body{body}
{
}

function_define::function_define()
  : decl{}
  , body{}
{
}

} // namespace miko::ast
