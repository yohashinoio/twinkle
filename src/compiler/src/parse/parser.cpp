/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/pch/pch.hpp>
#include <maple/ast/ast_adapted.hpp>
#include <maple/parse/parser.hpp>
#include <maple/codegen/type.hpp>
#include <maple/parse/exception.hpp>

namespace x3     = boost::spirit::x3;
namespace fusion = boost::fusion;

namespace maple::parse
{

//===----------------------------------------------------------------------===//
// Error handling
//===----------------------------------------------------------------------===//

struct ErrorHandle {
  template <typename Iterator, typename Context>
  x3::error_handler_result on_error([[maybe_unused]] Iterator&       first,
                                    [[maybe_unused]] const Iterator& last,
                                    const x3::expectation_failure<Iterator>& x,
                                    Context const& context) const
  {
    auto& error_handler = x3::get<x3::error_handler_tag>(context).get();

    error_handler(
      x.where(),
      formatError("expected: " + boost::core::demangle(x.which().c_str())));

    return x3::error_handler_result::fail;
  }
};

//===----------------------------------------------------------------------===//
// Annotations
//===----------------------------------------------------------------------===//

// Tag used to get the position cache from the context.
struct PositionCacheTag;

struct AnnotatePosition {
  template <typename T, typename Iterator, typename Context>
  void on_success(const Iterator& first,
                  const Iterator& last,
                  T&              ast,
                  const Context&  ctx)
  {
    auto&& position_cache = x3::get<PositionCacheTag>(ctx);
    position_cache.annotate(ast, first, last);
  }
};

//===----------------------------------------------------------------------===//
// Semantic actions
//===----------------------------------------------------------------------===//

namespace action
{

const auto assignAttrToVal = [](auto&& ctx) {
  x3::_val(ctx) = std::move(x3::_attr(ctx));
};

template <typename T>
struct assignToValAs {
  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const -> std::enable_if_t<
    std::is_same_v<Ast, ast::BinOp> || std::is_same_v<Ast, ast::Pipeline>>
  {
    Ast ast{std::move(x3::_val(ctx)),
            std::move(fusion::at_c<0>(x3::_attr(ctx))),
            std::move(fusion::at_c<1>(x3::_attr(ctx)))};

    assignAstToVal(ctx, std::move(ast));
  }

  template <typename Ctx, typename Ast = T>
  auto operator()(const Ctx& ctx) const
    // clang-format off
    -> std::enable_if_t<
         std::is_same_v<Ast, ast::Conversion>
      || std::is_same_v<Ast, ast::Subscript>
      || std::is_same_v<Ast, ast::FunctionCall>
      || std::is_same_v<Ast, ast::MemberAccess>
    >
  // clang-format on
  {
    Ast ast{
      std::move(x3::_val(ctx)),
      std::move(fusion::at_c<1>(x3::_attr(ctx))),
    };

    assignAstToVal(ctx, std::move(ast));
  };

private:
  template <typename Ctx>
  void assignAstToVal(const Ctx& ctx, T&& ast) const
  {
    // FIXME: There is a bug that causes the position to be annotated one
    // position further than the actual position.
    auto&& position_cache = x3::get<PositionCacheTag>(ctx);
    position_cache.annotate(ast,
                            x3::_where(ctx).begin(),
                            x3::_where(ctx).end());

    x3::_val(ctx) = std::forward<T>(ast);
  }
};

} // namespace action

//===----------------------------------------------------------------------===//
// Syntax
//===----------------------------------------------------------------------===//

namespace syntax
{

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-shift-op-parentheses"

using x3::unicode::lit;
using x3::unicode::char_;
using x3::unicode::string;

template <typename T>
using UnicodeSymbols
  = x3::symbols_parser<boost::spirit::char_encoding::unicode, T>;

//===----------------------------------------------------------------------===//
// Symbol table
//===----------------------------------------------------------------------===//

struct VariableQualifierSymbolsTag : UnicodeSymbols<VariableQual> {
  VariableQualifierSymbolsTag()
  {
    // clang-format off
    add
      (U"mut", VariableQual::mutable_)
    ;
    // clang-format on
  }
} variable_qualifier_symbols;

struct FunctionLinkageSymbolsTag : UnicodeSymbols<Linkage> {
  FunctionLinkageSymbolsTag()
  {
    // clang-format off
    add
      (U"private", Linkage::internal)
    ;
    // clang-format on
  }
} function_linkage_symbols;

struct EscapeCharSymbolsTag : UnicodeSymbols<char32_t> {
  EscapeCharSymbolsTag()
  {
    // clang-format off
    add
      (U"\\a", U'\a')
      (U"\\b", U'\b')
      (U"\\f", U'\f')
      (U"\\n", U'\n')
      (U"\\r", U'\r')
      (U"\\t", U'\t')
      (U"\\v", U'\v')
      (U"\\0", U'\0')
      (U"\\\\", U'\\')
      (U"\\\'", U'\'')
      (U"\\\"", U'\"')
    ;
    // clang-format on
  }
} escape_char_symbols;

//===----------------------------------------------------------------------===//
// Common rules
//===----------------------------------------------------------------------===//

// The reason for using x3::rule where a rule is not a recursive rule is to
// speed up compilation.

const auto identifier_internal
  = x3::rule<struct IdentifierInternalTag, std::u32string>{"identifier"}
= x3::raw
  [x3::lexeme[(x3::unicode::graph - (x3::unicode::digit | x3::unicode::punct)
               | lit(U"_"))
              >> *(x3::unicode::graph - x3::unicode::punct | lit(U"_"))]];

const auto identifier
  = x3::rule<struct IdentifierTag, ast::Identifier>{"identifier"}
= identifier_internal;

const auto variable_qualifier
  = x3::rule<struct VariableQualifierTag, VariableQual>{"variable qualifier"}
= variable_qualifier_symbols;

const auto function_linkage
  = x3::rule<struct FunctionLinkageTag, Linkage>{"function linkage"}
= function_linkage_symbols;

const auto binary_literal
  = x3::rule<struct BinaryLiteralTag, std::uint32_t>{"binary literal"}
= x3::lexeme[lit(U"0b") >> x3::uint_parser<std::uint32_t, 2>{}];

const auto octal_literal
  = x3::rule<struct OctalLiteralTag, std::uint32_t>{"octal literal"}
= x3::lexeme[lit(U"0") >> x3::uint_parser<std::uint32_t, 8>{}];

const auto hex_literal
  = x3::rule<struct HexLiteralTag, std::uint32_t>{"hexadecimal literal"}
= x3::lexeme[lit(U"0x") >> x3::uint_parser<std::uint32_t, 16>{}];

const auto uint_32bit
  = x3::rule<struct UnsignedInteger32Tag, std::uint32_t>{"integral number"}
= x3::uint32;

const auto int_32bit
  = x3::rule<struct SignedInteger32Tag, std::int32_t>{"integral number"}
= x3::int32;

const auto uint_64bit = x3::rule<struct UnsignedInteger64Tag,
                                 std::uint64_t>{"integral number (64bit)"}
= x3::uint64;

const auto int_64bit
  = x3::rule<struct SignedInteger64Tag, std::int64_t>{"integral number (64bit)"}
= x3::int64;

const auto float_64bit
  = x3::rule<struct Float64Tag,
             double>{"double precision floating point number"}
= boost::spirit::x3::
  real_parser<double, boost::spirit::x3::strict_real_policies<double>>{};

const auto boolean_literal
  = x3::rule<struct BooleanLiteralTag, bool>{"boolean literal"}
= lit(U"true") >> x3::attr(true) | lit(U"false") >> x3::attr(false);

const auto escape_char
  = x3::rule<struct EscapeCharTag, unsigned char>{"escape character"}
= lit(U"\\") >> x3::int_parser<char, 8, 1, 3>{}     // Octal
  | lit(U"\\x") >> x3::int_parser<char, 16, 2, 2>{} // Hexadecimal
  | escape_char_symbols;

const auto string_literal
  = x3::rule<struct StringLiteralTag, ast::StringLiteral>{"string literal"}
= x3::lexeme[lit(U"\"")
             >> *(char_ - (lit(U"\"") | x3::eol | lit(U"\\")) | escape_char)
             > lit(U"\"")];

const auto char_literal
  = x3::rule<struct CharLiteralTag, ast::CharLiteral>{"character literal"}
= lit(U"'") >> (char_ - (lit(U"'") | x3::eol | lit(U"\\")) | escape_char)
  > lit(U"'");

const auto type
  = x3::rule<struct TypeInternalTag, std::shared_ptr<codegen::Type>>{"type"}
= (-char_(U'*')
   >> identifier_internal /* TODO: support double (recursion) ptr */
   >> -(lit(U"[") >> x3::uint64 >> lit(U"]")))[([](auto&& ctx) {
    const auto type_in_utf32 = fusion::at_c<1>(x3::_attr(ctx));

    const auto type_kind = codegen::matchBuiltinType(type_in_utf32);

    std::shared_ptr<codegen::Type> base_type;
    if (type_kind)
      base_type = std::make_shared<codegen::BuiltinType>(*type_kind);
    else
      base_type = std::make_shared<codegen::StructType>(type_in_utf32);

    if (fusion::at_c<0>(x3::_attr(ctx))) {
      if (fusion::at_c<2>(x3::_attr(ctx))) {
        // Pointer array types.
        x3::_val(ctx) = std::make_shared<codegen::ArrayType>(
          std::make_shared<codegen::PointerType>(std::move(base_type)),
          *fusion::at_c<2>(x3::_attr(ctx)) /* Array size */);
        return;
      }

      // Pointer types.
      x3::_val(ctx)
        = std::make_shared<codegen::PointerType>(std::move(base_type));
      return;
    }

    if (fusion::at_c<2>(x3::_attr(ctx))) {
      // Array types.
      x3::_val(ctx) = std::make_shared<codegen::ArrayType>(
        std::move(base_type),
        *fusion::at_c<2>(x3::_attr(ctx)) /* Array size */);
      return;
    }

    // Fundamental (built-in) types.
    x3::_val(ctx) = std::move(base_type);
  })];

const auto attribute = x3::rule<struct AttrTag, ast::Attrs>{"attribute"}
= lit(U"[[") >> (identifier_internal % lit(U",")) > lit(U"]]");

//===----------------------------------------------------------------------===//
// Operator rules
//===----------------------------------------------------------------------===//

const auto assignment_operator = x3::rule<struct AssignmentOperatorTag,
                                          std::u32string>{"assignment operator"}
= string(U"=") | string(U"+=") | string(U"-=") | string(U"*=") | string(U"/=")
  | string(U"%=");

const auto equality_operator
  = x3::rule<struct EqualityOperatorTag, std::u32string>{"equality operator"}
= string(U"==") | string(U"!=");

const auto relational_operator = x3::rule<struct RelationalOperatorTag,
                                          std::u32string>{"relational operator"}
= string(U"<=") | string(U">=") /* <= and >= must come first */
  | string(U"<") | string(U">");

const auto additive_operator
  = x3::rule<struct AdditiveOperatorTag, std::u32string>{"additive operator"}
= (string(U"+") - string(U"+=")) | (string(U"-") - string(U"-="));

const auto pipeline_operator
  = x3::rule<struct PipelineOperatorTag, std::u32string>{"pipeline operator"}
= string(U"|>");

const auto multitive_operator
  = x3::rule<struct MultitiveOperatorTag, std::u32string>{"multitive operator"}
= (string(U"*") - string(U"*=")) | (string(U"/") - string(U"/="))
  | (string(U"%") - string(U"%="));

// logical not is excluded because it is a unary operator.
const auto binary_logical_operator
  = x3::rule<struct BinaryLogicalOperatorTag,
             std::u32string>{"binary logical operator"}
= string(U"&&") | string(U"||");

const auto unary_operator
  = x3::rule<struct UnaryOperatorTag, std::u32string>{"unary operator"}
= string(U"+") | string(U"-") | string(U"!") | string(U"*") | string(U"&")
  | string(U"sizeof");

//===----------------------------------------------------------------------===//
// Expression rules
//===----------------------------------------------------------------------===//

const x3::rule<struct ExprTag, ast::Expr>          expr{"expression"};
const x3::rule<struct BinaryLogicalTag, ast::Expr> binary_logical{
  "binary logical operation"};
const x3::rule<struct EqualTag, ast::Expr>    equal{"equality operation"};
const x3::rule<struct RelationTag, ast::Expr> relation{"relational operation"};
const x3::rule<struct PipelineTag, ast::Expr> pipeline{"pipeline operation"};
const x3::rule<struct AddTag, ast::Expr>      add{"addition operation"};
const x3::rule<struct MulTag, ast::Expr>      mul{"multiplication operation"};
const x3::rule<struct ConversionTag, ast::Expr>       conversion{"conversion"};
const x3::rule<struct UnaryInternalTag, ast::UnaryOp> unary_internal{
  "unary operation"};
const x3::rule<struct UnaryTag, ast::Expr>        unary{"unary operation"};
const x3::rule<struct MemberAccessTag, ast::Expr> member_access{
  "member access operation"};
const x3::rule<struct SubscriptTag, ast::Expr> subscript{"subscript operation"};
const x3::rule<struct ArgListTag, std::deque<ast::Expr>> arg_list{
  "argument list"};
const x3::rule<struct FunctionCallTag, ast::Expr> function_call{
  "function call"};
const x3::rule<struct PrimaryTag, ast::Expr> primary{"primary"};

//===----------------------------------------------------------------------===//
// Statement rules
//===----------------------------------------------------------------------===//

const x3::rule<struct InitListTag, ast::InitializerList> initializer_list{
  "initializer list"};
const x3::rule<struct InitializerTag, ast::Initializer> initializer{
  "initializer"};
const x3::rule<struct ExprStmtTag, ast::Expr> expr_stmt{"expression statement"};
const x3::rule<struct VariableDefTag, ast::VariableDef> variable_def{
  "variable definition"};
const x3::rule<struct AssignTag, ast::Assignment> assignment{
  "assignment statement"};
const x3::rule<struct PrefixIncOrDec, ast::PrefixIncAndDec> prefix_inc_or_dec{
  "prefix increment or decrement"};
const x3::rule<struct ReturnTag, ast::Return> _return{"return statement"};
const x3::rule<struct IfTag, ast::If>         _if{"if else statement"};
const x3::rule<struct LoopTag, ast::Loop>     _loop{"loop statement"};
const x3::rule<struct WhileTag, ast::While>   _while{"while statement"};
const x3::rule<struct ForTag, ast::For>       _for{"for statement"};
const x3::rule<struct StmtTag, ast::Stmt>     stmt{"statement"};

//===----------------------------------------------------------------------===//
// Top level rules
//===----------------------------------------------------------------------===//

const x3::rule<struct StructDeclTag, ast::StructDecl> struct_decl{
  "struct declaration"};
const x3::rule<struct VariableDefWithoutInit, ast::VariableDefWithoutInit>
  variable_def_without_init{"variable definition without initializer"};
const x3::rule<struct StructElementsTag, ast::StructElements> struct_elements{
  "struct elements"};
const x3::rule<struct StructDefTag, ast::StructDef> struct_def{
  "struct definition"};
const x3::rule<struct ParameterTag, ast::Parameter> parameter{"parameter"};
const x3::rule<struct ParameterListTag, ast::ParameterList> parameter_list{
  "parameter list"};
const x3::rule<struct FunctionProtoTag, ast::FunctionDecl> function_proto{
  "function prototype"};
const x3::rule<struct FunctionDeclTag, ast::FunctionDecl> function_decl{
  "function declaration"};
const x3::rule<struct FunctionDefTag, ast::FunctionDef> function_def{
  "function definition"};
const x3::rule<struct TopLevelTag, ast::TopLevel> top_level{"top level"};
const x3::rule<struct TopLevelWithAttrTag, ast::TopLevelWithAttr>
  top_level_with_attr{"top level"};

//===----------------------------------------------------------------------===//
// Comment rules
//===----------------------------------------------------------------------===//

const x3::rule<struct BlockCommentTag> block_comment{"block comment"};

//===----------------------------------------------------------------------===//
// Program rule
//===----------------------------------------------------------------------===//

const x3::rule<struct ProgramTag, ast::Program> program{"program"};

//===----------------------------------------------------------------------===//
// Expression rules definition
//===----------------------------------------------------------------------===//

const auto expr_def = binary_logical;

const auto binary_logical_def
  = equal[action::assignAttrToVal]
    >> *(binary_logical_operator > equal)[action::assignToValAs<ast::BinOp>{}];

const auto equal_def
  = relation[action::assignAttrToVal]
    >> *(equality_operator > relation)[action::assignToValAs<ast::BinOp>{}];

const auto relation_def
  = pipeline[action::assignAttrToVal]
    >> *(relational_operator > pipeline)[action::assignToValAs<ast::BinOp>{}];

const auto pipeline_def
  = add[action::assignAttrToVal]
    >> *(pipeline_operator > add)[action::assignToValAs<ast::Pipeline>{}];

const auto add_def
  = mul[action::assignAttrToVal]
    >> *(additive_operator > mul)[action::assignToValAs<ast::BinOp>{}];

const auto mul_def
  = conversion[action::assignAttrToVal]
    >> *(multitive_operator > conversion)[action::assignToValAs<ast::BinOp>{}];

// Replacing string(U"as") with lit(U"as") causes an error. I don't know why.
const auto conversion_def
  = unary[action::assignAttrToVal]
    >> *(string(U"as") > type)[action::assignToValAs<ast::Conversion>{}];

const auto unary_internal_def = unary_operator >> member_access;
const auto unary_def          = unary_internal | member_access;

// Replacing string(U".") with lit(U".") causes an error. I don't know why.
const auto member_access_def
  = subscript[action::assignAttrToVal]
    >> *(string(U".") > identifier)[action::assignToValAs<ast::MemberAccess>{}];

// Replacing string(U"[") with lit(U"[") causes an error. I don't know why.
const auto subscript_def
  = function_call[action::assignAttrToVal]
    >> *(string(U"[") > expr
         > lit(U"]"))[action::assignToValAs<ast::Subscript>{}];

const auto arg_list_def = -(expr % lit(U","));

// Replacing string(U"(") with lit(U"(") causes an error. I don't know why.
const auto function_call_def
  = primary[action::assignAttrToVal]
    >> *(string(U"(") > arg_list
         > lit(U")"))[action::assignToValAs<ast::FunctionCall>{}];

const auto primary_def
  = float_64bit | binary_literal | octal_literal | hex_literal | int_32bit
    | uint_32bit | int_64bit | uint_64bit | boolean_literal | string_literal
    | char_literal | identifier | (lit(U"(") > expr > lit(U")"));

BOOST_SPIRIT_DEFINE(expr)
BOOST_SPIRIT_DEFINE(binary_logical)
BOOST_SPIRIT_DEFINE(equal)
BOOST_SPIRIT_DEFINE(relation)
BOOST_SPIRIT_DEFINE(pipeline)
BOOST_SPIRIT_DEFINE(add)
BOOST_SPIRIT_DEFINE(mul)
BOOST_SPIRIT_DEFINE(conversion)
BOOST_SPIRIT_DEFINE(unary)
BOOST_SPIRIT_DEFINE(member_access)
BOOST_SPIRIT_DEFINE(subscript)
BOOST_SPIRIT_DEFINE(arg_list)
BOOST_SPIRIT_DEFINE(function_call)
BOOST_SPIRIT_DEFINE(unary_internal)
BOOST_SPIRIT_DEFINE(primary)

//===----------------------------------------------------------------------===//
// Statement rules definition
//===----------------------------------------------------------------------===//

const auto initializer_list_def = lit(U"{") > (expr % lit(U",")) > lit(U"}");

const auto initializer_def = expr | initializer_list;

const auto expr_stmt_def = expr;

const auto assignment_def = expr >> assignment_operator > expr;

const auto prefix_inc_or_dec_def = (string(U"++") | string(U"--")) > expr;

const auto variable_type
  = x3::rule<struct variable_type_tag,
             std::shared_ptr<codegen::Type>>{"variable type"}
= type - lit(U"void");

const auto variable_def_def = lit(U"let") > -variable_qualifier > identifier
                              > -(lit(U":") > variable_type)
                              > -(lit(U"=") > initializer);

const auto _return_def = lit(U"return") > -expr;

const auto _if_def
  = lit(U"if") > lit(U"(") > expr > lit(U")") > stmt > -(lit(U"else") > stmt);

const auto _loop_def = lit(U"loop") > stmt;

const auto _while_def = lit(U"while") > lit(U"(") > expr /* Condition */
                        > lit(U")") > stmt;

const auto _for_def = lit(U"for") > lit(U"(")
                      > -(assignment | variable_def) /* Init */
                      > lit(U";") > -expr            /* Condition */
                      > lit(U";") > -(prefix_inc_or_dec | assignment) /* Loop */
                      > lit(U")") > stmt;

const auto _break = x3::rule<struct BreakTag, ast::Break>{"break statement"}
= lit(U"break") >> x3::attr(ast::Break{});

const auto _continue
  = x3::rule<struct ContinueTag, ast::Continue>{"continue statement"}
= lit(U"continue") >> x3::attr(ast::Continue{});

const auto stmt_def = lit(U";")                         /* Null statement */
                      | lit(U"{") >> *stmt >> lit(U"}") /* Compound statement */
                      | _loop | _while | _for | _if | _break >> lit(U";")
                      | _continue >> lit(U";") | _return >> lit(U";")
                      | prefix_inc_or_dec >> lit(U";") | assignment >> lit(U";")
                      | variable_def >> lit(U";") | expr_stmt >> lit(U";");

BOOST_SPIRIT_DEFINE(initializer_list)
BOOST_SPIRIT_DEFINE(initializer)
BOOST_SPIRIT_DEFINE(expr_stmt)
BOOST_SPIRIT_DEFINE(variable_def)
BOOST_SPIRIT_DEFINE(assignment)
BOOST_SPIRIT_DEFINE(prefix_inc_or_dec)
BOOST_SPIRIT_DEFINE(_return)
BOOST_SPIRIT_DEFINE(_if)
BOOST_SPIRIT_DEFINE(_loop)
BOOST_SPIRIT_DEFINE(_while)
BOOST_SPIRIT_DEFINE(_for)
BOOST_SPIRIT_DEFINE(stmt)

//===----------------------------------------------------------------------===//
// Top level rules definition
//===----------------------------------------------------------------------===//

const auto struct_decl_def
  = lit(U"declare") >> lit(U"struct") > identifier > lit(U";");

const auto variable_def_without_init_def
  = lit(U"let") > identifier > lit(U":") > variable_type;

const auto struct_elements_def
  = *((variable_def_without_init > lit(U";")) | function_def);

const auto struct_def_def
  = lit(U"struct") > identifier > lit(U"{") > struct_elements > lit(U"}");

const auto parameter_def
  = (identifier > lit(U":") > -variable_qualifier > type > x3::attr(false))
    | lit(U"...")
        >> x3::attr(ast::Parameter{ast::Identifier{}, std::nullopt, {}, true});

const auto parameter_list_def = -(parameter % lit(U","));

const auto function_proto_def
  = (function_linkage | x3::attr(Linkage::external)) > identifier > lit(U"(")
    > parameter_list > lit(U")")
    > ((lit(U"->") > type)
       | x3::attr(std::make_shared<codegen::BuiltinType>(
         codegen::BuiltinTypeKind::void_)));

const auto function_decl_def
  = lit(U"declare") >> lit(U"func") > function_proto > lit(U";");

const auto function_def_def = lit(U"func") > function_proto > stmt;

const auto top_level_def
  = function_decl | function_def | struct_decl | struct_def;

const auto top_level_with_attr_def = -attribute >> top_level_def;

BOOST_SPIRIT_DEFINE(variable_def_without_init)
BOOST_SPIRIT_DEFINE(struct_elements)
BOOST_SPIRIT_DEFINE(struct_def)
BOOST_SPIRIT_DEFINE(struct_decl)
BOOST_SPIRIT_DEFINE(parameter)
BOOST_SPIRIT_DEFINE(parameter_list)
BOOST_SPIRIT_DEFINE(function_proto)
BOOST_SPIRIT_DEFINE(function_decl)
BOOST_SPIRIT_DEFINE(function_def)
BOOST_SPIRIT_DEFINE(top_level)
BOOST_SPIRIT_DEFINE(top_level_with_attr)

//===----------------------------------------------------------------------===//
// Comment rules definition
//===----------------------------------------------------------------------===//

const auto single_line_comment
  = x3::rule<struct SingleLineCommenTag>{"single line comment"}
= lit(U"//") >> *(char_ - x3::eol) >> (x3::eol | x3::eoi);

const auto block_comment_def
  = lit(U"/*") >> *(block_comment | (char_ - lit(U"*/"))) >> lit(U"*/");

const auto comment = x3::rule<struct CommentTag>{"comment"}
= single_line_comment | block_comment;

BOOST_SPIRIT_DEFINE(block_comment)

//===----------------------------------------------------------------------===//
// Skipper rules definition
//===----------------------------------------------------------------------===//

const auto skipper = x3::rule<struct SkipperTag>{"skipper"}
= x3::space | comment;

//===----------------------------------------------------------------------===//
// Program rules definition
//===----------------------------------------------------------------------===//

const auto program_def = *top_level_with_attr > x3::eoi;

BOOST_SPIRIT_DEFINE(program)

#pragma clang diagnostic pop

//===----------------------------------------------------------------------===//
// Common tags
//===----------------------------------------------------------------------===//

struct VariableIdentTag
  : ErrorHandle
  , AnnotatePosition {};

struct IdentifierTag
  : ErrorHandle
  , AnnotatePosition {};

struct StringLiteralTag
  : ErrorHandle
  , AnnotatePosition {};

struct CharLiteralTag
  : ErrorHandle
  , AnnotatePosition {};

struct TypeTag
  : ErrorHandle
  , AnnotatePosition {};

struct AttrTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Expression tags
//===----------------------------------------------------------------------===//

struct ExprTag
  : ErrorHandle
  , AnnotatePosition {};

struct BinaryLogical
  : ErrorHandle
  , AnnotatePosition {};

struct EqualTag
  : ErrorHandle
  , AnnotatePosition {};

struct RelationTag
  : ErrorHandle
  , AnnotatePosition {};

struct PipelineTag
  : ErrorHandle
  , AnnotatePosition {};

struct AddTag
  : ErrorHandle
  , AnnotatePosition {};

struct MulTag
  : ErrorHandle
  , AnnotatePosition {};

struct ConversionTag
  : ErrorHandle
  , AnnotatePosition {};

struct UnaryInternalTag
  : ErrorHandle
  , AnnotatePosition {};

struct UnaryTag
  : ErrorHandle
  , AnnotatePosition {};

struct MemberAccessTag
  : ErrorHandle
  , AnnotatePosition {};

struct SubscriptTag
  : ErrorHandle
  , AnnotatePosition {};

struct ArgListTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionCallTag
  : ErrorHandle
  , AnnotatePosition {};

struct PrimaryTag
  : ErrorHandle
  , AnnotatePosition {};

struct BlockExprTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Statement tags
//===----------------------------------------------------------------------===//

struct StmtTag
  : ErrorHandle
  , AnnotatePosition {};

struct InitListTag
  : ErrorHandle
  , AnnotatePosition {};

struct InitializerTag
  : ErrorHandle
  , AnnotatePosition {};

struct ExprStmtTag
  : ErrorHandle
  , AnnotatePosition {};

struct VariableDefTag
  : ErrorHandle
  , AnnotatePosition {};

struct AssignTag
  : ErrorHandle
  , AnnotatePosition {};

struct PrefixIncOrDec
  : ErrorHandle
  , AnnotatePosition {};

struct PrefixDecrement
  : ErrorHandle
  , AnnotatePosition {};

struct ReturnTag
  : ErrorHandle
  , AnnotatePosition {};

struct IfTag
  : ErrorHandle
  , AnnotatePosition {};

struct LoopTag
  : ErrorHandle
  , AnnotatePosition {};

struct WhileTag
  : ErrorHandle
  , AnnotatePosition {};

struct ForTag
  : ErrorHandle
  , AnnotatePosition {};

struct BreakTag
  : ErrorHandle
  , AnnotatePosition {};

struct ContinueTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Top level statement tags
//===----------------------------------------------------------------------===//

struct VariableDefWithoutInit
  : ErrorHandle
  , AnnotatePosition {};

struct StructElementsTag
  : ErrorHandle
  , AnnotatePosition {};

struct StructDefTag
  : ErrorHandle
  , AnnotatePosition {};

struct StructDeclTag
  : ErrorHandle
  , AnnotatePosition {};

struct ParameterTag
  : ErrorHandle
  , AnnotatePosition {};

struct ParameterListTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionProtoTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionDeclTag
  : ErrorHandle
  , AnnotatePosition {};

struct FunctionDefTag
  : ErrorHandle
  , AnnotatePosition {};

struct TopLevelTag
  : ErrorHandle
  , AnnotatePosition {};

struct TopLevelWithAttrTag
  : ErrorHandle
  , AnnotatePosition {};

//===----------------------------------------------------------------------===//
// Program tag
//===----------------------------------------------------------------------===//

struct ProgramTag
  : ErrorHandle
  , AnnotatePosition {};

} // namespace syntax

Parser::Parser(std::string&& input, std::filesystem::path&& file)
  : input{std::move(input)}
  , u32_first{cbegin(this->input)}
  , u32_last{cend(this->input)}
  , positions{u32_first, u32_last}
  , file{std::move(file)}
{
  parse();
}

void Parser::parse()
{
  x3::error_handler<InputIterator> error_handler{u32_first,
                                                 u32_last,
                                                 std::cerr,
                                                 file.string()};

  const auto parser = x3::with<x3::error_handler_tag>(std::ref(
    error_handler))[x3::with<PositionCacheTag>(positions)[syntax::program]];

  if (!x3::phrase_parse(u32_first, u32_last, parser, syntax::skipper, ast)
      || u32_first != u32_last) {
    // Some error occurred in parsing.
    throw ParseError{"compilation terminated."};
  }
}

} // namespace maple::parse
