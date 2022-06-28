/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _1d3d3a84_9536_11ec_b909_0242ac120002
#define _1d3d3a84_9536_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <lapis/pch/pch.hpp>
#include <lapis/unicode/unicode.hpp>
#include <lapis/support/kind.hpp>

namespace lapis
{

namespace codegen
{

enum class BuiltinTypeKind;

}

namespace ast
{

namespace x3 = boost::spirit::x3;

//===----------------------------------------------------------------------===//
// Common AST
//===----------------------------------------------------------------------===//

struct Identifier : x3::position_tagged {
  std::u32string name;

  explicit Identifier(std::u32string&& name)
    : name{std::move(name)}
  {
  }

  Identifier() = default;

  [[nodiscard]] std::string utf8() const
  {
    return unicode::utf32toUtf8(name);
  }

  [[nodiscard]] const std::u32string& utf32() const
  {
    return name;
  }
};

//===----------------------------------------------------------------------===//
// Type AST
//===----------------------------------------------------------------------===//

struct BuiltinType : x3::position_tagged {
  explicit BuiltinType(const codegen::BuiltinTypeKind kind)
    : kind{kind}
  {
  }

  BuiltinType() = default;

  codegen::BuiltinTypeKind kind;
};

struct UserDefinedType : x3::position_tagged {
  // Default constructor causes bugs
  // Cause unknown
  explicit UserDefinedType(Identifier&& name)
    : name{name}
  {
  }

  explicit UserDefinedType(const Identifier& name)
    : name{name}
  {
  }

  UserDefinedType() = default;

  Identifier name;
};

struct ArrayType;
struct PointerType;

using Type = boost::variant<boost::blank,
                            BuiltinType,
                            UserDefinedType,
                            boost::recursive_wrapper<ArrayType>,
                            boost::recursive_wrapper<PointerType>>;

struct ArrayType : x3::position_tagged {
  ArrayType(Type&& element_type, const std::uint64_t size)
    : element_type{std::move(element_type)}
    , size{size}
  {
  }

  Type          element_type;
  std::uint64_t size;
};

struct PointerType : x3::position_tagged {
  explicit PointerType(Type&& pointee_type) noexcept
    : pointee_type{std::move(pointee_type)}
  {
  }

  PointerType() = default;

  Type pointee_type;
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

struct BinOp;
struct UnaryOp;
struct Dereference;
struct FunctionCall;
struct Cast;
struct Subscript;
struct Pipeline;
struct MemberAccess;
struct ArrayLiteral;
struct UniformInit;

using Expr = boost::variant<boost::blank,
                            double,        // Floating point literals
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
                            boost::recursive_wrapper<Dereference>,
                            boost::recursive_wrapper<Subscript>,
                            boost::recursive_wrapper<FunctionCall>,
                            boost::recursive_wrapper<Cast>,
                            boost::recursive_wrapper<Pipeline>,
                            boost::recursive_wrapper<MemberAccess>,
                            boost::recursive_wrapper<ArrayLiteral>,
                            boost::recursive_wrapper<UniformInit>>;

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

  [[nodiscard]] std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind {
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

  [[nodiscard]] Kind kind() const
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

  UnaryOp(std::u32string&& op, Expr&& rhs)
    : op{std::move(op)}
    , rhs{std::move(rhs)}
  {
  }

  UnaryOp(const std::u32string& op, const Expr& rhs)
    : op{op}
    , rhs{rhs}
  {
  }

  UnaryOp() = default;

  [[nodiscard]] std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind {
    unknown,
    plus,       // Unary plus
    minus,      // Unary minus
    not_,       // Logical not
    address_of, // Address-of
    size_of,    // size-of
  };

  [[nodiscard]] Kind kind() const
  {
    if (op == U"+")
      return Kind::plus;
    if (op == U"-")
      return Kind::minus;
    if (op == U"!")
      return Kind::not_;
    if (op == U"&")
      return Kind::address_of;
    if (op == U"sizeof")
      return Kind::size_of;

    return Kind::unknown;
  }
};

struct Dereference : x3::position_tagged {
  Expr operand;

  explicit Dereference(Expr&& operand)
    : operand{std::move(operand)}
  {
  }
};

struct MemberAccess : x3::position_tagged {
  Expr lhs;
  Expr rhs;

  MemberAccess(Expr&& lhs, Expr&& rhs) noexcept
    : lhs{std::move(lhs)}
    , rhs{std::move(rhs)}
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
  std::deque<Expr> args;

  FunctionCall(Expr&& callee, std::deque<Expr>&& args) noexcept
    : callee{std::move(callee)}
    , args{std::move(args)}
  {
  }
};

struct Cast : x3::position_tagged {
  Expr lhs;
  Type as;

  Cast(Expr&& lhs, Type as) noexcept
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

struct ArrayLiteral : x3::position_tagged {
  std::vector<Expr> elements;
};

struct UniformInit : x3::position_tagged {
  Identifier        class_name;
  std::vector<Expr> initializer_list;
};

//===----------------------------------------------------------------------===//
// Statement AST
//===----------------------------------------------------------------------===//

struct Return : x3::position_tagged {
  std::optional<Expr> rhs;
};

struct VariableDef : x3::position_tagged {
  std::optional<VariableQual> qualifier;
  Identifier                  name;
  std::optional<Type>         type;
  std::optional<Expr>         initializer;
};

struct Assignment : x3::position_tagged {
  Expr           lhs; // Only assignable.
  std::u32string op;
  Expr           rhs;

  std::string operatorStr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind {
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

  enum class Kind {
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

struct Break : x3::position_tagged {};

struct Continue : x3::position_tagged {};

struct If;
struct Loop;
struct While;
struct For;

using Stmt = boost::make_recursive_variant<
  boost::blank,
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

using ForInitVariant = boost::variant<boost::blank, Assignment, VariableDef>;
using ForLoopVariant
  = boost::variant<boost::blank, PrefixIncAndDec, Assignment>;

struct For : x3::position_tagged {
  std::optional<ForInitVariant> init_stmt;
  std::optional<Expr>           cond_expr;
  std::optional<ForLoopVariant> loop_stmt;
  Stmt                          body;
};

//===----------------------------------------------------------------------===//
// Top level AST
//===----------------------------------------------------------------------===//

struct Parameter : x3::position_tagged {
  Identifier                  name;
  std::optional<VariableQual> qualifier;
  Type                        type;
  bool                        is_vararg;

  Parameter(Identifier&&                  name,
            std::optional<VariableQual>&& qualifier,
            Type&&                        type,
            const bool                    is_vararg) noexcept
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
  std::deque<Parameter> params;

  [[nodiscard]] const std::deque<Parameter>& operator*() const noexcept
  {
    return params;
  }

  const std::deque<Parameter>* operator->() const noexcept
  {
    return &params;
  }

  std::deque<Parameter>* operator->() noexcept
  {
    return &params;
  }
};

struct FunctionDecl : x3::position_tagged {
  Linkage       linkage;
  Identifier    name;
  ParameterList params;
  Type          return_type;
  Accessibility accessibility  = Accessibility::non_method;
  bool          is_constructor = false;
  bool          is_destructor  = false;
};

struct FunctionDef : x3::position_tagged {
  FunctionDef(FunctionDecl&& decl, Stmt&& body)
    : decl{std::move(decl)}
    , body(std::move(body))
  {
  }

  FunctionDef() = default;

  FunctionDecl decl;
  Stmt         body;
};

struct ClassDecl : x3::position_tagged {
  Identifier name;
};

struct VariableDefWithoutInit : x3::position_tagged {
  Identifier name;
  Type       type;
};

struct Constructor : x3::position_tagged {
  FunctionDecl decl;
  Stmt         body;
};

struct Destructor : x3::position_tagged {
  FunctionDecl decl;
  Stmt         body;
};

using StructMember = boost::variant<boost::blank,
                                    VariableDefWithoutInit,
                                    FunctionDef,
                                    Constructor,
                                    Destructor,
                                    Accessibility>;

using ClassMemberList = std::vector<StructMember>;

struct ClassDef : x3::position_tagged {
  Identifier      name;
  ClassMemberList members;
};

struct Typedef : x3::position_tagged {
  Identifier alias;
  Type       type;
};

using TopLevel = boost::variant<boost::blank,
                                FunctionDecl,
                                FunctionDef,
                                ClassDecl,
                                ClassDef,
                                Typedef>;

// Example: [[nodiscard, nomangle]]
using Attrs = std::vector<std::u32string>;

struct TopLevelWithAttr : x3::position_tagged {
  Attrs    attrs;
  TopLevel top_level;
};

using TranslationUnit = std::vector<TopLevelWithAttr>;

} // namespace ast

} // namespace lapis

#endif
