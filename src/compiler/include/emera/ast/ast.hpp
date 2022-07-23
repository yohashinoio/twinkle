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

#include <emera/pch/pch.hpp>
#include <emera/unicode/unicode.hpp>
#include <emera/codegen/kind.hpp>
#include <emera/support/kind.hpp>

// Note If the template argument of boost::variant exceeds 10, use
// boost::make_variant_over

namespace emera
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

struct Path : x3::position_tagged {
  std::u32string path;

  explicit Path(std::u32string&& path)
    : path{std::move(path)}
  {
  }

  Path() = default;

  [[nodiscard]] std::string utf8() const
  {
    return unicode::utf32toUtf8(path);
  }

  [[nodiscard]] const std::u32string& utf32() const
  {
    return path;
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
struct ReferenceType;

using Type = boost::variant<boost::blank,
                            BuiltinType,
                            UserDefinedType,
                            boost::recursive_wrapper<ArrayType>,
                            boost::recursive_wrapper<PointerType>,
                            boost::recursive_wrapper<ReferenceType>>;

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

struct ReferenceType : x3::position_tagged {
  explicit ReferenceType(Type&& refee_type) noexcept
    : refee_type{std::move(refee_type)}
  {
  }

  ReferenceType() = default;

  Type refee_type;
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

struct BuiltinMacro : x3::position_tagged {
  codegen::BuiltinMacroKind kind;
};

struct BinOp;
struct UnaryOp;
struct Reference;
struct New;
struct Delete;
struct Dereference;
struct FunctionCall;
struct Cast;
struct Subscript;
struct Pipeline;
struct MemberAccess;
struct ArrayLiteral;
struct ClassLiteral;

//===----------------------------------------------------------------------===//
// Expression Variant
//===----------------------------------------------------------------------===//

using ExprT0
  = boost::mpl::vector<boost::blank,
                       double,        // Floating point literals
                       std::uint32_t, // Unsigned integer literals (32bit)
                       std::int32_t,  // Signed integer literals (32bit)
                       std::uint64_t, // Unsinged integer litarals (64bit)
                       std::int64_t,  // Singed integer litarals (64bit)
                       bool,          // Boolean literals
                       StringLiteral,
                       CharLiteral,
                       Identifier>;

using ExprT1 = boost::mpl::push_back<ExprT0, BuiltinMacro>::type;

using ExprT2
  = boost::mpl::push_back<ExprT1, boost::recursive_wrapper<BinOp>>::type;

using ExprT3
  = boost::mpl::push_back<ExprT2, boost::recursive_wrapper<UnaryOp>>::type;

using ExprT4
  = boost::mpl::push_back<ExprT3, boost::recursive_wrapper<Dereference>>::type;

using ExprT5
  = boost::mpl::push_back<ExprT4, boost::recursive_wrapper<Subscript>>::type;

using ExprT6
  = boost::mpl::push_back<ExprT5, boost::recursive_wrapper<Cast>>::type;

using ExprT7
  = boost::mpl::push_back<ExprT6, boost::recursive_wrapper<Pipeline>>::type;

using ExprT8
  = boost::mpl::push_back<ExprT7, boost::recursive_wrapper<MemberAccess>>::type;

using ExprT9
  = boost::mpl::push_back<ExprT8, boost::recursive_wrapper<ArrayLiteral>>::type;

using ExprT10
  = boost::mpl::push_back<ExprT9, boost::recursive_wrapper<FunctionCall>>::type;

using ExprT11
  = boost::mpl::push_back<ExprT10,
                          boost::recursive_wrapper<ClassLiteral>>::type;

using ExprT12
  = boost::mpl::push_back<ExprT11, boost::recursive_wrapper<Reference>>::type;

using ExprT13
  = boost::mpl::push_back<ExprT12, boost::recursive_wrapper<New>>::type;

using ExprT14
  = boost::mpl::push_back<ExprT13, boost::recursive_wrapper<Delete>>::type;

using ExprTypes = ExprT14;

using Expr = boost::make_variant_over<ExprTypes>::type;

//===----------------------------------------------------------------------===//
// Expression AST
//===----------------------------------------------------------------------===//

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

  [[nodiscard]] std::string opstr() const
  {
    return unicode::utf32toUtf8(op);
  }

  enum class Kind {
    unknown,
    add,                 // Addition
    sub,                 // Subtraciton
    mul,                 // Multiplication
    div,                 // Division
    mod,                 // Modulo
    eq,                  // Equal to
    neq,                 // Not equal to
    lt,                  // Less than
    gt,                  // Greater than
    le,                  // Less than or equal to
    ge,                  // Greater than or equal to
    logical_and,         // Logical AND
    logical_or,          // Logical OR
    bitwise_shift_left,  // Bitwise shift left
    bitwise_shift_right, // Bitwise shift right
    bitwise_and,         // Bitwise AND
    bitwise_or,          // Bitwise OR
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
      return Kind::gt;
    if (op == U"<=")
      return Kind::le;
    if (op == U">=")
      return Kind::ge;
    if (op == U"&&")
      return Kind::logical_and;
    if (op == U"||")
      return Kind::logical_or;
    if (op == U"<<")
      return Kind::bitwise_shift_left;
    if (op == U">>")
      return Kind::bitwise_shift_right;
    if (op == U"&")
      return Kind::bitwise_and;
    if (op == U"|")
      return Kind::bitwise_or;

    return Kind::unknown;
  }
};

struct UnaryOp : x3::position_tagged {
  std::u32string op;
  Expr           operand;

  UnaryOp(std::u32string&& op, Expr&& rhs)
    : op{std::move(op)}
    , operand{std::move(rhs)}
  {
  }

  UnaryOp(const std::u32string& op, const Expr& rhs)
    : op{op}
    , operand{rhs}
  {
  }

  UnaryOp() = default;

  [[nodiscard]] std::string opstr() const
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

struct Reference : x3::position_tagged {
  Expr operand;
};

struct New : x3::position_tagged {
  Type              type;
  bool              with_init;
  std::vector<Expr> initializer;
};

struct Delete : x3::position_tagged {
  Expr operand;
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

struct ClassLiteral : x3::position_tagged {
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

  std::string opstr() const
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

struct PrefixIncrementDecrement : x3::position_tagged {
  std::u32string op;
  Expr           operand; // Only assignable.

  std::string opstr() const
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

//===----------------------------------------------------------------------===//
// Statement Variant
//===----------------------------------------------------------------------===//

using StmtT0 = boost::mpl::vector<boost::blank,
                                  // Compound statement
                                  std::vector<boost::recursive_variant_>,
                                  Expr,
                                  Return,
                                  VariableDef,
                                  Assignment,
                                  PrefixIncrementDecrement,
                                  Break,
                                  Continue,
                                  boost::recursive_wrapper<If>>;

using StmtT1
  = boost::mpl::push_back<StmtT0, boost::recursive_wrapper<Loop>>::type;

using StmtT2
  = boost::mpl::push_back<StmtT1, boost::recursive_wrapper<While>>::type;

using StmtT3
  = boost::mpl::push_back<StmtT2, boost::recursive_wrapper<For>>::type;

using StmtTypes = StmtT3;

using Stmt = boost::make_recursive_variant_over<StmtTypes>::type;

//===----------------------------------------------------------------------===//
// Statement AST
//===----------------------------------------------------------------------===//

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
  = boost::variant<boost::blank, PrefixIncrementDecrement, Assignment>;

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
  Identifier                       name;
  std::unordered_set<VariableQual> qualifier;
  Type                             type;
  bool                             is_vararg;

  Parameter(Identifier&&                       name,
            std::unordered_set<VariableQual>&& qualifier,
            Type&&                             type,
            const bool                         is_vararg) noexcept
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
  FunctionDef(const bool is_public, FunctionDecl&& decl, Stmt&& body)
    : is_public{is_public}
    , decl{std::move(decl)}
    , body(std::move(body))
  {
  }

  FunctionDef() = default;

  bool         is_public;
  FunctionDecl decl;
  Stmt         body;
};

struct ClassDecl : x3::position_tagged {
  Identifier name;
};

struct VariableDefWithoutInit : x3::position_tagged {
  std::optional<VariableQual> qualifier;
  Identifier                  name;
  Type                        type;
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
  bool            is_public;
  Identifier      name;
  ClassMemberList members;
};

struct Typedef : x3::position_tagged {
  Identifier alias;
  Type       type;
};

struct RelativeImport : x3::position_tagged {
  Path path;
};

using TopLevel = boost::variant<boost::blank,
                                FunctionDecl,
                                FunctionDef,
                                ClassDecl,
                                ClassDef,
                                Typedef,
                                RelativeImport>;

// Example: [[nodiscard, nomangle]]
using Attrs = std::vector<std::u32string>;

struct TopLevelWithAttr : x3::position_tagged {
  Attrs    attrs;
  TopLevel top_level;
};

using TranslationUnit = std::vector<TopLevelWithAttr>;

} // namespace ast

} // namespace emera

#endif
