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

struct nil {
};

struct unaryop;
struct binop;

using operand = boost::variant<nil,
                               int,
                               boost::recursive_wrapper<unaryop>,
                               boost::recursive_wrapper<binop>>;

struct unaryop : x3::position_tagged {
  std::string op;
  operand     rhs;

  unaryop(const std::string& op, const operand& rhs)
    : op{op}
    , rhs{rhs}
  {
  }
};

struct binop : x3::position_tagged {
  operand     lhs;
  std::string op;
  operand     rhs;

  binop(const operand& lhs, const std::string& op, const operand& rhs)
    : lhs{lhs}
    , op{op}
    , rhs{rhs}
  {
  }
};

using program = operand;

} // namespace miko::ast

#endif
