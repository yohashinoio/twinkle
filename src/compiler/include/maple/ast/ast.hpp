/**
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

#include <maple/pch/pch.hpp>
#include <maple/codegen/type.hpp>
#include <maple/unicode/unicode.hpp>

namespace maple::ast
{

namespace x3 = boost::spirit::x3;

struct Nil {
};

//===----------------------------------------------------------------------===//
// Expression AST
//===----------------------------------------------------------------------===//

struct StringLiteral : x3::position_tagged {
  using value_type = std::u32string::value_type;

  std::u32string str;
};

struct CharLiteral : x3::position_tagged {
  // Unicode code point.
  unicode::Codepoint ch;
};

struct Identifier : x3::position_tagged {
  std::u32string name;

  std::string utf8() const
  {
    return unicode::utf32toUtf8(name);
  }

  const std::u32string& utf32() const
  {
    return name;
  }
};

struct BinOp;
struct UnaryOp;
struct FunctionCall;
struct Conversion;
struct Subscript;
struct Pipeline;
struct MemberAccess;

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
                            boost::recursive_wrapper<Subscript>,
                            boost::recursive_wrapper<FunctionCall>,
                            boost::recursive_wrapper<Conversion>,
                            boost::recursive_wrapper<Pipeline>,
                            boost::recursive_wrapper<MemberAccess>>;

struct BinOp : x3::position_tagged {
  Expr           lhs;
  std::u32string op;
  Expr           rhs;

  BinOp(Expr&& lhs, std::u32string&& op, Expr&& rhs) noexcept
    : lhs{std::move(lhs)}
    , op{std::move(op)}
    , rhs{std::move(rhs)}
  {
  }

  std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind
  {
    unknown,
    add,         // Addition
    sub,         // Subtraciton
    mul,         // Multiplication
    div,         // Division
    mod,         // Modulo
    eq,          // Equal to
    neq,         // Not equal to
    lt,          // Less than
    gt,          // Greater than
    le,          // Less than or equal to
    ge,          // Greater than or equal to
    logical_and, // Logical AND
    logical_or,  // Logical OR
  };

  Kind kind() const
  {
    if (op == U"+")
      return Kind::add;
    if (op == U"-")
      return Kind::sub;
    if (op == U"*")
      return Kind::mul;
    if (op == U"/")
      return Kind::div;
    if (op == U"%")
      return Kind::mod;
    if (op == U"==")
      return Kind::eq;
    if (op == U"!=")
      return Kind::neq;
    if (op == U"<")
      return Kind::lt;
    if (op == U">")
      return Kind::ge;
    if (op == U"<=")
      return Kind::le;
    if (op == U">=")
      return Kind::ge;
    if (op == U"&&")
      return Kind::logical_and;
    if (op == U"||")
      return Kind::logical_or;

    return Kind::unknown;
  }
};

struct UnaryOp : x3::position_tagged {
  std::u32string op;
  Expr           rhs;

  std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind
  {
    unknown,
    plus,        // Unary plus
    minus,       // Unary minus
    not_,        // Logical not
    dereference, // Dereference
    address_of,  // Address-of
    size_of,     // size-of
  };

  Kind kind() const
  {
    if (op == U"+")
      return Kind::plus;
    if (op == U"-")
      return Kind::minus;
    if (op == U"!")
      return Kind::not_;
    if (op == U"*")
      return Kind::dereference;
    if (op == U"&")
      return Kind::address_of;
    if (op == U"sizeof")
      return Kind::size_of;

    return Kind::unknown;
  }
};

struct MemberAccess : x3::position_tagged {
  Expr       lhs;
  Identifier selected_element;

  MemberAccess(Expr&& lhs, Identifier&& selected_element) noexcept
    : lhs{std::move(lhs)}
    , selected_element{std::move(selected_element)}
  {
  }
};

struct Subscript : x3::position_tagged {
  Expr lhs;
  Expr subscript;

  Subscript(Expr&& lhs, Expr&& subscript) noexcept
    : lhs{std::move(lhs)}
    , subscript{std::move(subscript)}
  {
  }
};

struct FunctionCall : x3::position_tagged {
  Expr             callee;
  // Using deque instead of vector because of the possibility of adding an
  // element at the front.
  std::deque<Expr> args;

  FunctionCall(Expr&& callee, std::deque<Expr>&& args) noexcept
    : callee{std::move(callee)}
    , args{std::move(args)}
  {
  }
};

struct Conversion : x3::position_tagged {
  Expr                           lhs;
  std::shared_ptr<codegen::Type> as;

  Conversion(Expr&& lhs, std::shared_ptr<codegen::Type> as) noexcept
    : lhs{std::move(lhs)}
    , as{as}
  {
  }
};

struct Pipeline : x3::position_tagged {
  Expr           lhs;
  std::u32string op;
  Expr           rhs;

  Pipeline(Expr&& lhs, std::u32string&& op, Expr&& rhs) noexcept
    : lhs{std::move(lhs)}
    , op{std::move(op)}
    , rhs{std::move(rhs)}
  {
  }
};

//===----------------------------------------------------------------------===//
// Statement AST
//===----------------------------------------------------------------------===//

struct InitializerList : x3::position_tagged {
  // Initializers.
  std::vector<Expr> inits;
};

using Initializer = boost::variant<Expr, InitializerList>;

struct Return : x3::position_tagged {
  std::optional<Expr> rhs;
};

struct VariableDef : x3::position_tagged {
  std::optional<VariableQual>                   qualifier;
  Identifier                                    name;
  std::optional<std::shared_ptr<codegen::Type>> type;
  // Initializer.
  std::optional<Initializer>                    initializer;
};

struct Assignment : x3::position_tagged {
  Expr           lhs; // Only assignable.
  std::u32string op;
  Expr           rhs;

  std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind
  {
    unknown,
    direct, // Direct assignment
    add,    // Addition assignment
    sub,    // Subtraction assignment
    mul,    // Multiplication assignment
    div,    // Division assignment
    mod,    // Modulo assignment
  };

  Kind kind() const
  {
    if (op == U"=")
      return Kind::direct;
    if (op == U"+=")
      return Kind::add;
    if (op == U"-=")
      return Kind::sub;
    if (op == U"*=")
      return Kind::mul;
    if (op == U"/=")
      return Kind::div;
    if (op == U"%=")
      return Kind::mod;

    return Kind::unknown;
  }
};

struct PrefixIncAndDec : x3::position_tagged {
  std::u32string op;
  Expr           rhs; // Only assignable.

  std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind
  {
    unknown,
    increment,
    decrement,
  };

  Kind kind() const
  {
    if (op == U"++")
      return Kind::increment;
    if (op == U"--")
      return Kind::decrement;

    return Kind::unknown;
  }
};

struct Break : x3::position_tagged {
};

struct Continue : x3::position_tagged {
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
  Stmt body;
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
// Top level AST
//===----------------------------------------------------------------------===//

struct StructDecl : x3::position_tagged {
  Identifier name;
};

struct StructElement : x3::position_tagged {
  Identifier                     name;
  std::shared_ptr<codegen::Type> type;
};

struct StructDef : x3::position_tagged {
  Identifier                 name;
  std::vector<StructElement> elements;
};

struct Parameter : x3::position_tagged {
  Identifier                     name;
  std::optional<VariableQual>    qualifier;
  std::shared_ptr<codegen::Type> type;
  bool                           is_vararg;

  Parameter(Identifier&&                     name,
            std::optional<VariableQual>&&    qualifier,
            std::shared_ptr<codegen::Type>&& type,
            const bool                       is_vararg) noexcept
    : name{name}
    , qualifier{qualifier}
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
  Linkage                        linkage;
  Identifier                     name;
  ParameterList                  params;
  std::shared_ptr<codegen::Type> return_type;
};

struct FunctionDef : x3::position_tagged {
  FunctionDecl decl;
  Stmt         body;
};

using TopLevel
  = boost::variant<Nil, FunctionDecl, FunctionDef, StructDecl, StructDef>;

using Program = std::vector<TopLevel>;

} // namespace maple::ast

#endif
