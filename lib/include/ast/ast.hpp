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
#include <utils/type.hpp>

namespace x3 = boost::spirit::x3;

namespace maple
{

//===----------------------------------------------------------------------===//
// Abstract syntax tree
//===----------------------------------------------------------------------===//

namespace ast
{

struct Nil {};

//===----------------------------------------------------------------------===//
// Expression abstract syntax tree
//===----------------------------------------------------------------------===//

struct StringLiteral : x3::position_tagged {
  std::string str;
};

struct CharLiteral : x3::position_tagged {
  unsigned char ch;
};

struct VariableRef : x3::position_tagged {
  std::string name;
};

struct UnaryOp;
struct BinOp;
struct FunctionCall;
struct Conversion;
struct AddressOf;
struct Indirection;

using Expr = boost::variant<Nil,
                            std::uint32_t, // Unsigned integer literals (32bit)
                            std::int32_t,  // Signed integer literals (32bit)
                            std::uint64_t, // Unsinged integer litarals (64bit)
                            std::int64_t,  // Singed integer litarals (64bit)
                            bool,          // Boolean literals
                            StringLiteral,
                            CharLiteral,
                            VariableRef,
                            boost::recursive_wrapper<UnaryOp>,
                            boost::recursive_wrapper<BinOp>,
                            boost::recursive_wrapper<FunctionCall>,
                            boost::recursive_wrapper<Conversion>,
                            boost::recursive_wrapper<AddressOf>,
                            boost::recursive_wrapper<Indirection>>;

struct UnaryOp : x3::position_tagged {
  std::string op;
  Expr        rhs;
};

struct BinOp : x3::position_tagged {
  Expr        lhs;
  std::string op;
  Expr        rhs;

  BinOp(decltype(lhs)&& lhs, decltype(op)&& op, decltype(rhs)&& rhs)
    : lhs{std::move(lhs)}
    , op{std::move(op)}
    , rhs{std::move(rhs)}
  {
  }

  BinOp() noexcept
  {
  }
};

struct FunctionCall : x3::position_tagged {
  std::string       callee;
  std::vector<Expr> args;
};

struct Conversion : x3::position_tagged {
  Expr                  lhs;
  std::shared_ptr<Type> as;
};

struct AddressOf : x3::position_tagged {
  Expr lhs;
};

struct Indirection : x3::position_tagged {
  Expr lhs;
};

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree
//===----------------------------------------------------------------------===//

struct Return : x3::position_tagged {
  std::optional<Expr> rhs;
};

struct VariableDef : x3::position_tagged {
  std::optional<VariableQual>          qualifier;
  std::string                          name;
  std::optional<std::shared_ptr<Type>> type;
  std::optional<Expr>                  initializer;
};

struct Assignment : x3::position_tagged {
  Expr        lhs; // Only assignable.
  std::string op;
  Expr        rhs;
};

struct Break : x3::position_tagged {
  std::string tmp;
};

struct Continue : x3::position_tagged {
  std::string tmp;
};

struct If;
struct Loop;
struct While;
struct For;

using Stmt = boost::make_recursive_variant<
  Nil,
  std::vector<boost::recursive_variant_>, // Compound statement
  Expr,
  Return,
  VariableDef,
  Assignment,
  Break,
  Continue,
  boost::recursive_wrapper<If>,
  boost::recursive_wrapper<Loop>,
  boost::recursive_wrapper<While>,
  boost::recursive_wrapper<For>>::type;

using CompoundStmt = std::vector<Stmt>;

struct If : x3::position_tagged {
  Expr                condition;
  Stmt                then_statement;
  std::optional<Stmt> else_statement;
};

struct Loop : x3::position_tagged {
  std::string tmp;
  Stmt        body;
};

struct While : x3::position_tagged {
  Expr cond_expr;
  Stmt body;
};

using ForInit = boost::variant<Assignment, VariableDef>;

struct For : x3::position_tagged {
  std::optional<ForInit>    init_stmt;
  std::optional<Expr>       cond_expr;
  std::optional<Assignment> loop_stmt;
  Stmt                      body;
};

//===----------------------------------------------------------------------===//
// Top level abstract syntax tree
//===----------------------------------------------------------------------===//

struct Parameter : x3::position_tagged {
  std::optional<VariableQual> qualifier;
  std::string                 name;
  std::shared_ptr<Type>       type;
  bool                        is_vararg;

  Parameter(decltype(qualifier)&&     qualifier,
            decltype(name)&&          name,
            decltype(type)            type,
            const decltype(is_vararg) is_vararg)
    : qualifier{qualifier}
    , name{name}
    , type{type}
    , is_vararg{is_vararg}
  {
  }

  Parameter() noexcept
  {
  }
};

struct ParameterList : x3::position_tagged {
  std::vector<Parameter> params;

  [[nodiscard]] const Parameter& operator[](const std::size_t idx) const
  {
    return params.at(idx);
  }

  [[nodiscard]] const std::vector<Parameter>& operator*() const noexcept
  {
    return params;
  }

  [[nodiscard]] std::size_t length() const noexcept
  {
    return params.size();
  }
};

struct FunctionDecl : x3::position_tagged {
  std::optional<Linkage> linkage;
  std::string            name;
  ParameterList          params;
  std::shared_ptr<Type>  return_type;
};

struct FunctionDef : x3::position_tagged {
  FunctionDecl decl;
  Stmt         body;
};

using TopLevel = boost::variant<Nil, FunctionDecl, FunctionDef>;

using Program = std::vector<TopLevel>;

} // namespace ast
} // namespace maple

#endif
