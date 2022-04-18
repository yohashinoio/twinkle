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
#include <unicode/unicode.hpp>

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
  // Some compilers will error if there is no value_type,
  // because x3::rule using this AST uses iterator.
  using value_type = std::string;

  std::string str;
};

struct CharLiteral : x3::position_tagged {
  // Unicode code point.
  unicode::Codepoint ch;
};

struct Identifier : x3::position_tagged {
  std::u32string name;

  const std::u32string& operator*() const noexcept
  {
    return name;
  }
};

struct BinOp;
struct UnaryOp;
struct FunctionCall;
struct Conversion;

using Expr = boost::variant<Nil,
                            std::uint32_t, // Unsigned integer literals (32bit)
                            std::int32_t,  // Signed integer literals (32bit)
                            std::uint64_t, // Unsinged integer litarals (64bit)
                            std::int64_t,  // Singed integer litarals (64bit)
                            bool,          // Boolean literals
                            StringLiteral,
                            CharLiteral,
                            Identifier,
                            boost::recursive_wrapper<BinOp>,
                            boost::recursive_wrapper<UnaryOp>,
                            boost::recursive_wrapper<FunctionCall>,
                            boost::recursive_wrapper<Conversion>>;

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

  enum class Kind : unsigned char {
    unknown,
    add, // Addition
    sub, // Subtraciton
    mul, // Multiplication
    div, // Division
    mod, // Modulo
    eq,  // Equal to
    neq, // Not equal to
    lt,  // Less than
    gt,  // Greater than
    le,  // Less than or equal to
    ge,  // Greater than or equal to
  };

  Kind kind() const
  {
    if (op == "+")
      return Kind::add;
    if (op == "-")
      return Kind::sub;
    if (op == "*")
      return Kind::mul;
    if (op == "/")
      return Kind::div;
    if (op == "%")
      return Kind::mod;
    if (op == "==")
      return Kind::eq;
    if (op == "!=")
      return Kind::neq;
    if (op == "<")
      return Kind::lt;
    if (op == ">")
      return Kind::ge;
    if (op == "<=")
      return Kind::le;
    if (op == ">=")
      return Kind::ge;

    return Kind::unknown;
  }
};

struct UnaryOp : x3::position_tagged {
  std::string op;
  Expr        rhs;

  enum class Kind : unsigned char {
    unknown,
    plus,        // Unary plus
    minus,       // Unary minus
    indirection, // Indirection
    address_of,  // Address-of
    not_,        // Logical not
  };

  Kind kind() const
  {
    if (op == "+")
      return Kind::plus;
    if (op == "-")
      return Kind::minus;
    if (op == "*")
      return Kind::indirection;
    if (op == "&")
      return Kind::address_of;
    if (op == "!")
      return Kind::not_;

    return Kind::unknown;
  }
};

struct FunctionCall : x3::position_tagged {
  Identifier        callee;
  std::vector<Expr> args;
};

struct Conversion : x3::position_tagged {
  Expr                  lhs;
  std::shared_ptr<Type> as;
};

//===----------------------------------------------------------------------===//
// Statement abstract syntax tree
//===----------------------------------------------------------------------===//

struct InitList : x3::position_tagged {
  // Initializers.
  std::vector<Expr> inits;
};

using Initializer = boost::variant<Expr, InitList>;

struct Return : x3::position_tagged {
  std::optional<Expr> rhs;
};

struct VariableDef : x3::position_tagged {
  std::optional<VariableQual>          qualifier;
  Identifier                           name;
  std::optional<std::shared_ptr<Type>> type;
  // Initializer.
  std::optional<Initializer>           initializer;
};

struct Assignment : x3::position_tagged {
  Expr        lhs; // Only assignable.
  std::string op;
  Expr        rhs;
};

struct PrefixIncAndDec : x3::position_tagged {
  std::string op;
  Expr        rhs; // Only assignable.
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
  PrefixIncAndDec,
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

using ForInitVariant = boost::variant<Assignment, VariableDef>;
using ForLoopVariant = boost::variant<PrefixIncAndDec, Assignment>;

struct For : x3::position_tagged {
  std::optional<ForInitVariant> init_stmt;
  std::optional<Expr>           cond_expr;
  std::optional<ForLoopVariant> loop_stmt;
  Stmt                          body;
};

//===----------------------------------------------------------------------===//
// Top level abstract syntax tree
//===----------------------------------------------------------------------===//

struct Parameter : x3::position_tagged {
  std::optional<VariableQual> qualifier;
  Identifier                  name;
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
  std::shared_ptr<Type>  return_type;
  std::optional<Linkage> linkage;
  Identifier             name;
  ParameterList          params;
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
