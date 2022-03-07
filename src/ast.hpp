//
//  ast.hpp
//
//  Copyright (c) 2022 The Miko Authors.
//  MIT License
//

#ifndef _1d3d3a84_9536_11ec_b909_0242ac120002
#define _1d3d3a84_9536_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "pch.hpp"

namespace x3 = boost::spirit::x3;

namespace miko::ast
{

struct nil {};

////////////////
// expression //
////////////////
struct unaryop;
struct binop;
struct variable;
struct function_call;
struct assignment;

using expression
  = boost::variant<nil,
                   int,
                   boost::recursive_wrapper<unaryop>,
                   boost::recursive_wrapper<binop>,
                   boost::recursive_wrapper<variable> /* I don't know why, but
                                                         it has to be wrapped in
                                                         recursive_wrapper. */
                   ,
                   boost::recursive_wrapper<function_call>>;

struct unaryop : x3::position_tagged {
  std::string op;
  expression  rhs;

  unaryop(const std::string& op, const expression& rhs)
    : op{op}
    , rhs{rhs}
  {
  }

  unaryop()
    : op{}
    , rhs{}
  {
  }
};

struct binop : x3::position_tagged {
  expression  lhs;
  std::string op;
  expression  rhs;

  binop(const expression& lhs, const std::string& op, const expression& rhs)
    : lhs{lhs}
    , op{op}
    , rhs{rhs}
  {
  }

  binop()
    : lhs{}
    , op{}
    , rhs{}
  {
  }
};

struct variable : x3::position_tagged {
  std::string name;

  explicit variable(const std::string& name)
    : name{name}
  {
  }

  variable()
    : name{}
  {
  }
};

struct function_call : x3::position_tagged {
  std::string             callee;
  std::vector<expression> args;

  explicit function_call(const std::string&             callee,
                         const std::vector<expression>& args)
    : callee{callee}
    , args{args}
  {
  }

  function_call()
    : callee{}
    , args{args}
  {
  }
};

///////////////
// statement //
///////////////
struct return_statement;
struct variable_def;

using statement
  = boost::variant<nil, expression, return_statement, variable_def>;
using compound_statement = std::vector<statement>;

struct variable_def {
  std::string               name;
  std::optional<expression> initializer;

  explicit variable_def(const std::string&               name,
                        const std::optional<expression>& initializer)
    : name{name}
    , initializer{initializer}
  {
  }

  variable_def()
    : name{}
    , initializer{}
  {
  }
};

struct return_statement {
  expression rhs;

  explicit return_statement(const expression& rhs)
    : rhs{rhs}
  {
  }

  return_statement()
    : rhs{}
  {
  }
};

/////////////
// program //
/////////////
struct function_decl;
struct function_def;

using program = std::vector<boost::variant<nil, function_decl, function_def>>;

struct function_decl : x3::position_tagged {
  std::string              name;
  std::vector<std::string> args;

  explicit function_decl(const std::string&              name,
                         const std::vector<std::string>& args)
    : name{name}
    , args{args}
  {
  }

  function_decl()
    : name{}
    , args{}
  {
  }
};

struct function_def : x3::position_tagged {
  function_decl      decl;
  compound_statement body;

  function_def(const function_decl& decl, const compound_statement& body)
    : decl{decl}
    , body{body}
  {
  }

  function_def()
    : decl{}
    , body{}
  {
  }
};

} // namespace miko::ast

#endif
