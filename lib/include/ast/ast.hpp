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
  // Some compilers will error if there is no value_type,
  // because x3::rule using this AST uses iterator.
  using value_type = std::string;

  std::string str;
};

struct CharLiteral : x3::position_tagged {
  unsigned char ch;
};

struct Identifier : x3::position_tagged {
  std::string name;
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

  bool isAddition() const
  {
    return op == "+";
  }

  bool isSubtraction() const
  {
    return op == "-";
  }

  bool isMultiplication() const
  {
    return op == "*";
  }

  bool isDivision() const
  {
    return op == "/";
  }

  bool isModulo() const
  {
    return op == "%";
  }

  bool isEqual() const
  {
    return op == "==";
  }

  bool isNotEqual() const
  {
    return op == "!=";
  }

  bool isLessThan() const
  {
    return op == "<";
  }

  bool isGreaterThan() const
  {
    return op == ">";
  }

  bool isLessOrEqual() const
  {
    return op == "<=";
  }

  bool isGreaterOrEqual() const
  {
    return op == ">=";
  }
};

struct UnaryOp : x3::position_tagged {
  std::string op;
  Expr        rhs;

  bool isUnaryPlus() const
  {
    return op == "+";
  }

  bool isUnaryMinus() const
  {
    return op == "-";
  }

  bool isIndirection() const
  {
    return op == "*";
  }

  bool isAddressOf() const
  {
    return op == "&";
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
  std::string                          name;
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
  std::shared_ptr<Type>  return_type;
  std::optional<Linkage> linkage;
  std::string            name;
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
