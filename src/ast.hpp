//
//  ast.hpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
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

struct unaryop;
struct binop;
struct variable;
struct function_call;

using expression = boost::variant<nil,
                                  int,
                                  boost::recursive_wrapper<unaryop>,
                                  boost::recursive_wrapper<binop>,
                                  boost::recursive_wrapper<variable>,
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
  std::string callee;
  // TODO: arguments

  explicit function_call(const std::string& callee)
    : callee{callee}
  {
  }

  function_call()
    : callee{}
  {
  }
};

struct return_statement;

using statement          = boost::variant<nil, expression, return_statement>;
using compound_statement = std::vector<statement>;

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

struct function_decl;
struct function_def;

using toplevel = boost::variant<nil, function_decl, function_def>;

struct function_decl : x3::position_tagged {
  std::string name;

  explicit function_decl(const std::string& name)
    : name{name}
  {
  }

  function_decl()
    : name{}
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

using program = std::vector<toplevel>;

} // namespace miko::ast

#endif
