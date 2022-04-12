/**
 * codegen.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/codegen.hpp>
#include <utils/type.hpp>
#include <utils/format.hpp>
#include <cassert>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Utilities
//===----------------------------------------------------------------------===//

struct Variable {
  Variable(llvm::AllocaInst* pointer,
           const bool        is_mutable,
           const bool        is_signed) noexcept
    : pointer{pointer}
    , is_mutable{is_mutable}
    , is_signed{is_signed}
  {
  }

  llvm::AllocaInst* getAllocaInst() const noexcept
  {
    return pointer;
  }

  bool isMutable() const noexcept
  {
    return is_mutable;
  }

  bool isSigned() const noexcept
  {
    return is_signed;
  }

private:
  llvm::AllocaInst* pointer;
  bool              is_mutable;
  bool              is_signed;
};

struct SymbolTable {
  [[nodiscard]] std::optional<Variable>
  operator[](const std::string& name) const noexcept
  try {
    return named_values.at(name);
  }
  catch (const std::out_of_range&) {
    return std::nullopt;
  }

  // Regist stands for register.
  void regist(const std::string& name, Variable info)
  {
    named_values.insert({name, info});
  }

  // Returns true if the variable is already registered, false otherwise.
  bool exists(const std::string& name) const
  {
    return named_values.contains(name);
  }

  // For debug.
  void printTable() const
  {
    for (const auto& r : named_values)
      std::cout << r.first << ' ';
    std::endl(std::cout);
  }

private:
  std::unordered_map<std::string, Variable> named_values;
};

// Class that wraps llvm::Value.
// Made to handle signs, etc.
struct Value {
  Value(llvm::Value* value, const bool is_signed) noexcept
    : value{value}
    , is_signed{is_signed}
  {
  }

  explicit Value(llvm::Value* value) noexcept
    : value{value}
    , is_signed{false}
  {
  }

  Value() noexcept = default;

  [[nodiscard]] llvm::Value* getValue() const noexcept
  {
    return value;
  }

  [[nodiscard]] bool isSigned() const noexcept
  {
    return is_signed;
  }

  [[nodiscard]] bool isInteger() const
  {
    return value->getType()->isIntegerTy();
  }

  [[nodiscard]] explicit operator bool() const noexcept
  {
    return value;
  }

private:
  llvm::Value* value;
  bool         is_signed;
};

// Create an alloca instruction in the entry block of
// the function.
// This is used for variables etc.
[[nodiscard]] llvm::AllocaInst*
create_entry_block_alloca(llvm::Function*    func,
                          const std::string& var_name,
                          llvm::Type*        type)
{
  return llvm::IRBuilder<>{&func->getEntryBlock(),
                           func->getEntryBlock().begin()}
    .CreateAlloca(type, nullptr, var_name);
}

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CodeGenerator::Context& ctx, SymbolTable& scope)
    : ctx{ctx}
    , scope{scope}
  {
  }

  [[nodiscard]] Value operator()(ast::Nil) const
  {
    unreachable();
  }

  // 32bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint32_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node), false};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt32Ty(), node), true};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), node), false};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt64Ty(), node), true};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {
      ctx.int1ToBool(llvm::ConstantInt::get(ctx.builder.getInt1Ty(), node)),
      false};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return Value{ctx.builder.CreateGlobalStringPtr(node.str, ".str")};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Char literal is u8.
    return {llvm::ConstantInt::get(ctx.builder.getInt8Ty(), node.ch), false};
  }

  [[nodiscard]] Value operator()(const ast::UnaryOp& node) const
  {
    auto const rhs = boost::apply_visitor(*this, node.rhs);

    if (!rhs.getValue()) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate right-hand side")};
    }

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to (0 - x).
      return Value{ctx.builder.CreateSub(
        llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
        rhs.getValue())};
    }

    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown operator '%s' detected", node.op))};
  }

  [[nodiscard]] Value operator()(const ast::BinOp& node) const
  {
    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (!lhs) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate left-hand side",
                        false)};
    }

    if (!rhs) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate right-hand side",
                        false)};
    }

    // Implicit conversions.
    if (const auto lhs_bitwidth
        = lhs.getValue()->getType()->getIntegerBitWidth(),
        rhs_bitwidth = rhs.getValue()->getType()->getIntegerBitWidth();
        lhs_bitwidth != rhs_bitwidth) {
      const auto max_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

      const auto as = ctx.builder.getIntNTy(max_bitwidth);

      const auto as_is_signed
        = lhs_bitwidth == max_bitwidth ? lhs.isSigned() : rhs.isSigned();

      if (lhs_bitwidth == max_bitwidth) {
        rhs = {ctx.builder.CreateIntCast(rhs.getValue(), as, as_is_signed),
               as_is_signed};
      }
      else {
        lhs = {ctx.builder.CreateIntCast(lhs.getValue(), as, as_is_signed),
               as_is_signed};
      }
    }

    if (lhs.getValue()->getType() != rhs.getValue()->getType()) {
      throw std::runtime_error{ctx.formatError(
        ctx.positions.position_of(node),
        "both operands to a binary operator are not of the same type",
        false)};
    }

    // If either one of them is signed, the result is also signed.
    const auto result_is_signed
      = lhs.isSigned() || rhs.isSigned() ? true : false;

    // Addition.
    if (node.op == "+") {
      return {ctx.builder.CreateAdd(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }

    // Subtraction.
    if (node.op == "-") {
      return {ctx.builder.CreateSub(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }

    // Multiplication.
    if (node.op == "*") {
      return {ctx.builder.CreateMul(lhs.getValue(), rhs.getValue()),
              result_is_signed};
    }

    // Division.
    if (node.op == "/") {
      if (result_is_signed) {
        return {ctx.builder.CreateSDiv(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
      else {
        return {ctx.builder.CreateUDiv(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
    }

    // Modulo.
    if (node.op == "%") {
      if (result_is_signed) {
        return {ctx.builder.CreateSRem(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
      else {
        return {ctx.builder.CreateURem(lhs.getValue(), rhs.getValue()),
                result_is_signed};
      }
    }

    // Equal.
    if (node.op == "==") {
      return Value{
        ctx.int1ToBool(ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                                              lhs.getValue(),
                                              rhs.getValue()))};
    }

    // Not equal.
    if (node.op == "!=") {
      return Value{
        ctx.int1ToBool(ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                                              lhs.getValue(),
                                              rhs.getValue()))};
    }

    // Less than.
    if (node.op == "<") {
      return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SLT : llvm::ICmpInst::ICMP_ULT,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Greater than.
    if (node.op == ">") {
      return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SGT : llvm::ICmpInst::ICMP_UGT,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Less or equal.
    if (node.op == "<=") {
      return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SLE : llvm::ICmpInst::ICMP_ULE,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Greater or equal.
    if (node.op == ">=") {
      return Value{ctx.int1ToBool(ctx.builder.CreateICmp(
        result_is_signed ? llvm::ICmpInst::ICMP_SGE : llvm::ICmpInst::ICMP_UGE,
        lhs.getValue(),
        rhs.getValue()))};
    }

    // Unsupported binary operators detected.
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      format("unknown operator '%s' detected", node.op),
                      false)};
  }

  [[nodiscard]] Value operator()(const ast::VariableRef& node) const
  {
    auto variable = scope[node.name];

    if (!variable) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        format("unknown variable '%s' referenced", node.name))};
    }

    return {
      ctx.builder.CreateLoad(variable->getAllocaInst()->getAllocatedType(),
                             variable->getAllocaInst()),
      variable->isSigned()};
  }

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const
  {
    auto const callee_func = ctx.module->getFunction(node.callee);

    if (!callee_func) {
      throw std::runtime_error{ctx.formatError(
        ctx.positions.position_of(node),
        format("unknown function '%s' referenced", node.callee))};
    }

    if (!callee_func->isVarArg()
        && callee_func->arg_size() != node.args.size()) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        format("incorrect arguments passed"))};
    }

    std::vector<llvm::Value*> args_value;
    for (std::size_t i = 0, size = node.args.size(); i != size; ++i) {
      args_value.push_back(
        boost::apply_visitor(*this, node.args[i]).getValue());

      if (!args_value.back()) {
        throw std::runtime_error{ctx.formatError(
          ctx.positions.position_of(node),
          format("argument set failed in call to the function '%s'",
                 node.callee))};
      }
    }

    // Verify arguments
    for (std::size_t idx = 0; auto&& arg : callee_func->args()) {
      if (args_value[idx++]->getType() != arg.getType()) {
        throw std::runtime_error{
          ctx.formatError(ctx.positions.position_of(node),
                          format("incompatible type for argument %d of '%s'",
                                 idx + 1,
                                 node.callee))};
      }
    }

    return Value{ctx.builder.CreateCall(callee_func, args_value)};
  }

  [[nodiscard]] Value operator()(const ast::Conversion& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate left-hand side")};
    }

    // TODO: Support for non-integers and non-pointers.
    if (node.as->getType(ctx.builder)->isIntegerTy()) {
      return {ctx.builder.CreateIntCast(lhs.getValue(),
                                        node.as->getType(ctx.builder),
                                        node.as->isSigned()),
              node.as->isSigned()};
    }
    else if (node.as->isPointer()) {
      // FIXME: I would like to prohibit this in the regular cast because it is
      // a dangerous cast.
      return {ctx.builder.CreatePointerCast(lhs.getValue(),
                                            node.as->getType(ctx.builder)),
              node.as->isSigned()};
    }
    else {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "cannot be converted to the specified type")};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::AddressOf& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate right-hand side")};
    }

    return Value{llvm::getPointerOperand(lhs.getValue())};
  }

  [[nodiscard]] Value operator()(const ast::Indirection& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    if (!lhs) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate right-hand side")};
    }

    auto const lhs_type = lhs.getValue()->getType();

    if (!lhs_type->isPointerTy()) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "unary '*' requires pointer operand")};
    }

    return {
      ctx.builder.CreateLoad(lhs_type->getPointerElementType(), lhs.getValue()),
      lhs.isSigned()};
  }

private:
  CodeGenerator::Context& ctx;

  SymbolTable& scope;
};

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

void codegen_statement(const ast::Stmt&        statement,
                       const SymbolTable&      scope,
                       CodeGenerator::Context& ctx,
                       llvm::AllocaInst*       retvar,
                       llvm::BasicBlock*       end_bb,
                       llvm::BasicBlock*       break_bb,
                       llvm::BasicBlock*       continue_bb);

struct StmtVisitor : public boost::static_visitor<void> {
  StmtVisitor(CodeGenerator::Context& ctx,
              SymbolTable&            scope,
              llvm::AllocaInst*       retvar,
              llvm::BasicBlock*       end_bb,
              llvm::BasicBlock*       break_bb,
              llvm::BasicBlock*       continue_bb)
    : ctx{ctx}
    , scope{scope}
    , retvar{retvar}
    , end_bb{end_bb}
    , break_bb{break_bb}
    , continue_bb{continue_bb}
  {
  }

  void operator()(ast::Nil) const
  {
    // Empty statement, so not processed.
  }

  void operator()(const ast::CompoundStmt& node) const
  {
    codegen_statement(node, scope, ctx, retvar, end_bb, break_bb, continue_bb);
  }

  void operator()(const ast::Expr& node) const
  {
    if (!boost::apply_visitor(ExprVisitor{ctx, scope}, node)) {
      throw std::runtime_error{
        format_error_message(ctx.file.string(),
                             "failed to generate expression statement")};
    }
  }

  void operator()(const ast::Return& node) const
  {
    if (node.rhs) {
      auto const retval
        = boost::apply_visitor(ExprVisitor{ctx, scope}, *node.rhs);

      if (ctx.builder.GetInsertBlock()->getParent()->getReturnType()
          != retval.getValue()->getType()) {
        throw std::runtime_error{
          ctx.formatError(ctx.positions.position_of(node),
                          "incompatible type for result type")};
      }

      if (!retval) {
        throw std::runtime_error{
          ctx.formatError(ctx.positions.position_of(node),
                          "failed to generate return value")};
      }

      ctx.builder.CreateStore(retval.getValue(), retvar);
    }

    ctx.builder.CreateBr(end_bb);
  }

  void operator()(const ast::VariableDef& node) const
  {
    if (!node.type.has_value() && !node.initializer.has_value()) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "type inference requires an initializer")};
    }

    if (scope.exists(node.name)) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        format("redefinition of '%s'", node.name))};
    }

    auto const func = ctx.builder.GetInsertBlock()->getParent();

    const auto regist
      = [&](llvm::AllocaInst* const alloca, const bool is_signed) {
          if (!node.qualifier) {
            // Consttant variable.
            scope.regist(node.name, {alloca, false, is_signed});
          }
          else if (*node.qualifier == VariableQual::mutable_) {
            // Mutable variable.
            scope.regist(node.name, {alloca, true, is_signed});
          }
        };

    if (node.type) {
      const auto& type_info = **node.type;

      auto const alloca
        = create_entry_block_alloca(func,
                                    node.name,
                                    type_info.getType(ctx.builder));

      if (node.initializer) {
        auto const init_value
          = boost::apply_visitor(ExprVisitor{ctx, scope}, *node.initializer);

        if (!init_value) {
          throw std::runtime_error{ctx.formatError(
            ctx.positions.position_of(node),
            format("failed to generate initializer for '%s'", node.name))};
        }

        if (type_info.getType(ctx.builder)
            != init_value.getValue()->getType()) {
          throw std::runtime_error{ctx.formatError(
            ctx.positions.position_of(node),
            "initializer type and variable type are different")};
        }

        ctx.builder.CreateStore(init_value.getValue(), alloca);
      }

      regist(alloca, type_info.isSigned());
    }
    else {
      // Type inference.
      auto const init_value
        = boost::apply_visitor(ExprVisitor{ctx, scope}, *node.initializer);

      if (!init_value) {
        throw std::runtime_error{ctx.formatError(
          ctx.positions.position_of(node),
          format("failed to generate initializer for '%s'", node.name))};
      }

      auto const alloca
        = create_entry_block_alloca(func,
                                    node.name,
                                    init_value.getValue()->getType());

      ctx.builder.CreateStore(init_value.getValue(), alloca);

      regist(alloca, init_value.isSigned());
    }
  }

  void operator()(const ast::Assignment& node) const
  {
    if (node.op == "=" || node.op == "+=" || node.op == "-=" || node.op == "*="
        || node.op == "/=" || node.op == "%=") {
      const auto lhs
        = gen_assignable_value_from_expr(node.lhs,
                                         ctx.positions.position_of(node));

      auto const rhs = boost::apply_visitor(ExprVisitor{ctx, scope}, node.rhs);

      if (!rhs) {
        throw std::runtime_error{
          ctx.formatError(ctx.positions.position_of(node),
                          "failed to generate right-hand side")};
      }

      if (lhs.getValue()->getType()->getPointerElementType()
          != rhs.getValue()->getType()) {
        throw std::runtime_error{ctx.formatError(
          ctx.positions.position_of(node),
          "both operands to a binary operator are not of the same type")};
      }

      auto const lhs_value = ctx.builder.CreateLoad(
        lhs.getValue()->getType()->getPointerElementType(),
        lhs.getValue());

      // Direct assignment.
      if (node.op == "=")
        ctx.builder.CreateStore(rhs.getValue(), lhs.getValue());

      // Addition assignment.
      if (node.op == "+=") {
        ctx.builder.CreateStore(
          ctx.builder.CreateAdd(lhs_value, rhs.getValue()),
          lhs.getValue());
      }

      // Subtraction assignment.
      if (node.op == "-=") {
        ctx.builder.CreateStore(
          ctx.builder.CreateSub(lhs_value, rhs.getValue()),
          lhs.getValue());
      }

      // Multiplication assignment.
      if (node.op == "*=") {
        ctx.builder.CreateStore(
          ctx.builder.CreateMul(lhs_value, rhs.getValue()),
          lhs.getValue());
      }

      const auto result_is_signed
        = rhs.isSigned() || lhs.isSigned() ? true : false;

      // Division assignment.
      if (node.op == "/=") {
        auto const assign_value
          = result_is_signed
              ? ctx.builder.CreateSDiv(lhs_value, rhs.getValue())
              : ctx.builder.CreateUDiv(lhs_value, rhs.getValue());

        ctx.builder.CreateStore(assign_value, lhs.getValue());
      }

      // Modulo assignment.
      if (node.op == "%=") {
        auto const assign_value
          = result_is_signed
              ? ctx.builder.CreateSRem(lhs_value, rhs.getValue())
              : ctx.builder.CreateURem(lhs_value, rhs.getValue());

        ctx.builder.CreateStore(assign_value, lhs.getValue());
      }
    }
  }

  void operator()(const ast::PrefixIncAndDec& node) const
  {
    const auto rhs
      = gen_assignable_value_from_expr(node.rhs,
                                       ctx.positions.position_of(node));

    auto const rhs_value = ctx.builder.CreateLoad(
      rhs.getValue()->getType()->getPointerElementType(),
      rhs.getValue());

    if (node.op == "++") {
      ctx.builder.CreateStore(
        ctx.builder.CreateAdd(rhs_value,
                              llvm::ConstantInt::get(rhs_value->getType(), 1)),
        rhs.getValue());
    }

    if (node.op == "--") {
      ctx.builder.CreateStore(
        ctx.builder.CreateSub(rhs_value,
                              llvm::ConstantInt::get(rhs_value->getType(), 1)),
        rhs.getValue());
    }
  }

  void operator()(const ast::If& node) const
  {
    auto const cond_value
      = boost::apply_visitor(ExprVisitor{ctx, scope}, node.condition);

    if (!cond_value) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "invalid condition in if statement")};
    }

    // Convert condition to a bool by comparing non-equal to 0.
    auto const cond = ctx.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value.getValue(),
      llvm::ConstantInt::get(
        BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.builder),
        0));

    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const then_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    auto const else_bb = llvm::BasicBlock::Create(ctx.context);

    auto const merge_bb = llvm::BasicBlock::Create(ctx.context);

    ctx.builder.CreateCondBr(cond, then_bb, else_bb);

    // Then statement codegen.
    ctx.builder.SetInsertPoint(then_bb);

    codegen_statement(node.then_statement,
                      scope,
                      ctx,
                      retvar,
                      end_bb,
                      break_bb,
                      continue_bb);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(merge_bb);

    // Else statement codegen.
    func->getBasicBlockList().push_back(else_bb);
    ctx.builder.SetInsertPoint(else_bb);

    if (node.else_statement) {
      codegen_statement(*node.else_statement,
                        scope,
                        ctx,
                        retvar,
                        end_bb,
                        break_bb,
                        continue_bb);
    }

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(merge_bb);

    func->getBasicBlockList().push_back(merge_bb);
    ctx.builder.SetInsertPoint(merge_bb);
  }

  void operator()(const ast::Loop& node) const
  {
    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const body_bb = llvm::BasicBlock::Create(ctx.context, "", func);

    auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context);

    ctx.builder.CreateBr(body_bb);
    ctx.builder.SetInsertPoint(body_bb);

    codegen_statement(node.body,
                      scope,
                      ctx,
                      retvar,
                      end_bb,
                      loop_end_bb,
                      body_bb);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(body_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    ctx.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::While& node) const
  {
    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const cond_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    auto const body_bb = llvm::BasicBlock::Create(ctx.context);

    auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context);

    ctx.builder.CreateBr(cond_bb);
    ctx.builder.SetInsertPoint(cond_bb);

    auto const cond_value
      = boost::apply_visitor(ExprVisitor{ctx, scope}, node.cond_expr);

    if (!cond_value) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate condition expression")};
    }

    auto const cond = ctx.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value.getValue(),
      llvm::ConstantInt::get(
        BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.builder),
        0));

    ctx.builder.CreateCondBr(cond, body_bb, loop_end_bb);

    func->getBasicBlockList().push_back(body_bb);
    ctx.builder.SetInsertPoint(body_bb);

    codegen_statement(node.body,
                      scope,
                      ctx,
                      retvar,
                      end_bb,
                      loop_end_bb,
                      cond_bb);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(cond_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    ctx.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::For& node) const
  {
    if (node.init_stmt)
      boost::apply_visitor(*this, *node.init_stmt);

    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const cond_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    auto const loop_bb = llvm::BasicBlock::Create(ctx.context);
    auto const body_bb = llvm::BasicBlock::Create(ctx.context);

    auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context);

    ctx.builder.CreateBr(cond_bb);
    ctx.builder.SetInsertPoint(cond_bb);

    if (node.cond_expr) {
      auto const cond_value
        = boost::apply_visitor(ExprVisitor{ctx, scope}, *node.cond_expr);

      if (!cond_value) {
        throw std::runtime_error{
          ctx.formatError(ctx.positions.position_of(node),
                          "failed to generate condition expression")};
      }

      auto const cond = ctx.builder.CreateICmp(
        llvm::ICmpInst::ICMP_NE,
        cond_value.getValue(),
        llvm::ConstantInt::get(
          BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.builder),
          0));

      ctx.builder.CreateCondBr(cond, body_bb, loop_end_bb);
    }
    else {
      // If condition is absent, unconditionally true.
      ctx.builder.CreateCondBr(
        llvm::ConstantInt::get(ctx.builder.getInt1Ty(), true),
        body_bb,
        loop_end_bb);
    }

    func->getBasicBlockList().push_back(body_bb);
    ctx.builder.SetInsertPoint(body_bb);

    codegen_statement(node.body,
                      scope,
                      ctx,
                      retvar,
                      end_bb,
                      loop_end_bb,
                      loop_bb);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(loop_bb);

    func->getBasicBlockList().push_back(loop_bb);
    ctx.builder.SetInsertPoint(loop_bb);

    // Generate loop statement.
    if (node.loop_stmt) {
      // Since variables will not declared, there is no need to create a new
      // scope.
      boost::apply_visitor(
        StmtVisitor{ctx, scope, retvar, end_bb, loop_end_bb, loop_bb},
        *node.loop_stmt);
    }

    ctx.builder.CreateBr(cond_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    ctx.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(ast::Break) const
  {
    // Whether in a loop.
    if (break_bb)
      ctx.builder.CreateBr(break_bb);
  }

  void operator()(ast::Continue) const
  {
    // Whether in a loop.
    if (continue_bb)
      ctx.builder.CreateBr(continue_bb);
  }

private:
  Value gen_assignable_value_from_expr(
    const ast::Expr&                                  node,
    const boost::iterator_range<maple::InputIterator> position) const
  {
    Value value;

    if (node.type() == typeid(ast::VariableRef)) {
      auto& var_ref_node = boost::get<ast::VariableRef>(node);

      auto variable = scope[var_ref_node.name];

      if (!variable) {
        // Unknown variable name.
        throw std::runtime_error{ctx.formatError(
          position,
          format("unknown variable name '%s'", var_ref_node.name))};
      }

      if (!variable->isMutable()) {
        // Assignment of read-only variable.
        throw std::runtime_error{ctx.formatError(
          position,
          format("assignment of read-only variable '%s'", var_ref_node.name))};
      }

      value = {variable->getAllocaInst(), variable->isSigned()};
    }
    else if (node.type() == typeid(ast::Indirection)) {
      auto const indirection_node = boost::get<ast::Indirection>(node);

      value
        = boost::apply_visitor(ExprVisitor{ctx, scope}, indirection_node.lhs);
    }
    else
      value = boost::apply_visitor(ExprVisitor{ctx, scope}, node);

    if (!value) {
      throw std::runtime_error{
        ctx.formatError(position, "failed to generate left-hand side")};
    }

    if (!value.getValue()->getType()->isPointerTy()) {
      throw std::runtime_error{
        ctx.formatError(position, "left-hand side requires assignable")};
    }

    return value;
  }

  CodeGenerator::Context& ctx;

  SymbolTable& scope;

  // Used to combine returns into one.
  llvm::AllocaInst* retvar;
  llvm::BasicBlock* end_bb;

  // If not in loop, nullptr.
  llvm::BasicBlock* break_bb; // Basic block transitioned by break statement.
  llvm::BasicBlock*
    continue_bb; // Basic block transitioned by continue statement.
};

void codegen_statement(const ast::Stmt&        statement,
                       const SymbolTable&      scope,
                       CodeGenerator::Context& ctx,
                       llvm::AllocaInst*       retvar,
                       llvm::BasicBlock*       end_bb,
                       llvm::BasicBlock*       break_bb,
                       llvm::BasicBlock*       continue_bb)
{
  SymbolTable new_scope = scope;

  // Compound statement
  if (statement.type() == typeid(ast::CompoundStmt)) {
    auto& statements = boost::get<ast::CompoundStmt>(statement);

    for (const auto& r : statements) {
      boost::apply_visitor(
        StmtVisitor{ctx, new_scope, retvar, end_bb, break_bb, continue_bb},
        r);

      // If a terminator is present, subsequent code generation is
      // terminated.
      if (ctx.builder.GetInsertBlock()->getTerminator())
        break;
    }

    return;
  }

  // Other than compound statement
  boost::apply_visitor(
    StmtVisitor{ctx, new_scope, retvar, end_bb, break_bb, continue_bb},
    statement);
}

//===----------------------------------------------------------------------===//
// Top level statement visitor
//===----------------------------------------------------------------------===//

struct TopLevelVisitor : public boost::static_visitor<llvm::Function*> {
  TopLevelVisitor(CodeGenerator::Context&            ctx,
                  llvm::legacy::FunctionPassManager& fp_manager)
    : ctx{ctx}
    , fp_manager{fp_manager}
  {
  }

  llvm::Function* operator()(ast::Nil) const
  {
    throw std::runtime_error{format_error_message(
      ctx.file.string(),
      "a function was executed that did not predict execution")};
  }

  llvm::Function* operator()(const ast::FunctionDecl& node) const
  {
    auto&& ps = *node.params;

    if (ps.size() && ps.at(0).is_vararg) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "requires a named argument before '...'")};
    }

    bool is_vararg = false;
    for (const auto& r : ps) {
      if (r.is_vararg) {
        if (is_vararg) {
          throw std::runtime_error{
            ctx.formatError(ctx.positions.position_of(node),
                            "cannot have multiple variable arguments")};
        }
        else
          is_vararg = true;
      }
    }

    const auto named_params_length
      = is_vararg ? node.params.length() - 1 : node.params.length();
    std::vector<llvm::Type*> param_types(named_params_length);

    for (std::size_t i = 0; i != named_params_length; ++i) {
      const auto& param_type = node.params[i].type;
      param_types.at(i)      = param_type->getType(ctx.builder);
    }

    auto const func_type
      = llvm::FunctionType::get(node.return_type->getType(ctx.builder),
                                param_types,
                                is_vararg);

    llvm::Function* func;
    if (!node.linkage) {
      // External linkage.
      func = llvm::Function::Create(func_type,
                                    llvm::Function::ExternalLinkage,
                                    node.name,
                                    *ctx.module);
    }
    else if (node.linkage == Linkage::internal) {
      // Internal linkage.
      func = llvm::Function::Create(func_type,
                                    llvm::Function::InternalLinkage,
                                    node.name,
                                    *ctx.module);
    }

    // Set names for all arguments.
    for (std::size_t idx = 0; auto&& arg : func->args())
      arg.setName(node.params[idx++].name);

    return func;
  }

  llvm::Function* operator()(const ast::FunctionDef& node) const
  {
    auto func = ctx.module->getFunction(node.decl.name);

    if (!func)
      func = this->operator()(node.decl);

    if (!func) {
      throw std::runtime_error{ctx.formatError(
        ctx.positions.position_of(node),
        format("failed to create function %s", node.decl.name))};
    }

    SymbolTable argument_values;

    auto const entry_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    ctx.builder.SetInsertPoint(entry_bb);

    for (auto&& arg : func->args()) {
      const auto& param_node = node.decl.params[arg.getArgNo()];

      // Create an alloca for this variable.
      auto const alloca
        = create_entry_block_alloca(func,
                                    arg.getName().str(),
                                    param_node.type->getType(ctx.builder));

      // Store the initial value into the alloca.
      ctx.builder.CreateStore(&arg, alloca);

      // Add arguments to variable symbol table.
      if (!param_node.qualifier) {
        // constant variable.
        argument_values.regist(arg.getName().str(),
                               {alloca, false, param_node.type->isSigned()});
      }
      else if (*param_node.qualifier == VariableQual::mutable_) {
        // mutable variable.
        argument_values.regist(arg.getName().str(),
                               {alloca, true, param_node.type->isSigned()});
      }
    }

    // Used to combine returns into one.
    auto const end_bb = llvm::BasicBlock::Create(ctx.context);
    auto const retvar
      = node.decl.return_type->getKind() == BuiltinTypeKind::void_
          ? nullptr
          : create_entry_block_alloca(
            func,
            "",
            node.decl.return_type->getType(ctx.builder));

    codegen_statement(node.body,
                      argument_values,
                      ctx,
                      retvar,
                      end_bb,
                      nullptr,
                      nullptr);

    // If there is no return, returns undef.
    if (!ctx.builder.GetInsertBlock()->getTerminator()
        && node.decl.return_type->getKind() != BuiltinTypeKind::void_) {
      // Return 0 specially for main.
      if (node.decl.name == "main") {
        ctx.builder.CreateStore(
          llvm::ConstantInt::getSigned(func->getReturnType(), 0),
          retvar);
        ctx.builder.CreateBr(end_bb);
      }
      else {
        ctx.builder.CreateStore(llvm::UndefValue::get(func->getReturnType()),
                                retvar);
        ctx.builder.CreateBr(end_bb);
      }
    }

    // Inserts a terminator if the function returning void does not have
    // one.
    if (node.decl.return_type->getKind() == BuiltinTypeKind::void_
        && !ctx.builder.GetInsertBlock()->getTerminator()) {
      ctx.builder.CreateBr(end_bb);
    }

    // Return.
    func->getBasicBlockList().push_back(end_bb);
    ctx.builder.SetInsertPoint(end_bb);

    if (retvar) {
      auto const retval
        = ctx.builder.CreateLoad(retvar->getAllocatedType(), retvar);
      ctx.builder.CreateRet(retval);
    }
    else {
      // Function that returns void.
      ctx.builder.CreateRet(nullptr);
    }

    std::string              em;
    llvm::raw_string_ostream os{em};
    if (llvm::verifyFunction(*func, &os)) {
      func->eraseFromParent();

      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node), os.str())};
    }

    fp_manager.run(*func);

    return func;
  }

private:
  CodeGenerator::Context& ctx;

  llvm::legacy::FunctionPassManager& fp_manager;
};

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

CodeGenerator::Context::Context(llvm::LLVMContext&           context,
                                const std::filesystem::path& file,
                                const PositionCache&         positions)
  : context{context}
  , module{std::make_unique<llvm::Module>(file.filename().string(), context)}
  , builder{context}
  , file{file}
  , positions{positions}
{
}

[[nodiscard]] llvm::Value*
CodeGenerator::Context::int1ToBool(llvm::Value* value)
{
  const BuiltinType as{BuiltinTypeKind::bool_};

  return llvm::CastInst::CreateIntegerCast(value,
                                           as.getType(builder),
                                           as.isSigned(),
                                           "",
                                           builder.GetInsertBlock());
}

[[nodiscard]] std::string CodeGenerator::Context::formatError(
  const boost::iterator_range<InputIterator> pos,
  const std::string_view                     message,
  const bool                                 with_code)
{
  // Calculate line numbers.
  std::size_t rows = 0;
  for (auto iter = pos.begin();; --iter) {
    if (*iter == '\n')
      ++rows;

    if (iter == positions.first()) {
      ++rows;
      break;
    }
  }

  std::ostringstream ss;

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    ss << "In file " << file.string() << ", line " << rows << ":\n"
       << COLOR_RED "error: " COLOR_DEFAULT << message << '\n';
  }
#else
  ss << "In file " << file.string() << ", line " << line << ":\n"
     << "error: " << message << '\n';
#endif

  if (with_code) {
    std::for_each(pos.begin(), pos.end(), [&](auto&& ch) { ss << ch; });

    // TODO:
    // ss << "\n^_";
  }

  return ss.str();
}

CodeGenerator::CodeGenerator(const std::string_view               argv_front,
                             std::vector<parse::Parser::Result>&& asts,
                             const bool                           opt,
                             const llvm::Reloc::Model relocation_model)
  : argv_front{argv_front}
  , context{std::make_unique<llvm::LLVMContext>()}
  , relocation_model{relocation_model}
  , asts{asts}
{
  results.reserve(asts.size());

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  initTargetTripleAndMachine();

  for (auto [ast, positions, file] : asts) {
    Context ctx{*context, file, positions};

    llvm::legacy::FunctionPassManager fp_manager{ctx.module.get()};

    if (opt) {
      fp_manager.add(llvm::createInstructionCombiningPass());
      fp_manager.add(llvm::createReassociatePass());
      fp_manager.add(llvm::createGVNPass());
      fp_manager.add(llvm::createCFGSimplificationPass());
      fp_manager.add(llvm::createPromoteMemoryToRegisterPass());
      fp_manager.add(llvm::createInstructionCombiningPass());
      fp_manager.add(llvm::createReassociatePass());
    }

    fp_manager.doInitialization();

    ctx.module->setTargetTriple(target_triple);
    ctx.module->setDataLayout(target_machine->createDataLayout());

    codegen(ast, ctx, fp_manager);

    results.push_back({std::move(ctx.module), std::move(file)});
  }
}

void CodeGenerator::emitLlvmIRFiles()
{
  for (auto it = results.begin(), last = results.end(); it != last; ++it) {
    const auto& file = std::get<std::filesystem::path>(*it);

    std::error_code      ostream_ec;
    llvm::raw_fd_ostream os{file.stem().string() + ".ll",
                            ostream_ec,
                            llvm::sys::fs::OpenFlags::OF_None};

    if (ostream_ec) {
      throw std::runtime_error{format_error_message(
        argv_front,
        format("%s: %s", file.string(), ostream_ec.message()))};
    }

    std::get<std::unique_ptr<llvm::Module>>(*it)->print(os, nullptr);
  }
}

void CodeGenerator::emitAssemblyFiles()
{
  emitFiles(llvm::CGFT_AssemblyFile);
}

void CodeGenerator::emitObjectFiles()
{
  emitFiles(llvm::CGFT_ObjectFile);
}

[[nodiscard]] int CodeGenerator::doJIT()
{
  assert(!jit_compiled);

  jit_compiled = true;

  auto jit_expected = jit::JitCompiler::create();
  if (auto err = jit_expected.takeError()) {
    throw std::runtime_error{
      format_error_message(argv_front, llvm::toString(std::move(err)), true)};
  }

  auto jit = std::move(*jit_expected);

  auto [front_module, file] = std::move(results.front());

  // Link all modules.
  for (auto it = results.begin() + 1, last = results.end(); it != last; ++it) {
    auto [module, file] = std::move(*it);

    if (llvm::Linker::linkModules(*front_module, std::move(module))) {
      throw std::runtime_error{
        format_error_message(argv_front,
                             format("%s: Could not link", file.string()))};
    }
  }

  if (auto err
      = jit->addModule({std::move(front_module), std::move(context)})) {
    throw std::runtime_error{
      format_error_message(file.string(), llvm::toString(std::move(err)))};
  }

  auto symbol_expected = jit->lookup("main");
  if (auto err = symbol_expected.takeError()) {
    throw std::runtime_error{
      format_error_message(argv_front, "symbol main could not be found")};
  }

  auto       symbol = *symbol_expected;
  auto const main_addr
    = reinterpret_cast<int (*)(/* TODO: command line arguments */)>(
      symbol.getAddress());

  // Run main.
  return main_addr();
}

void CodeGenerator::codegen(const ast::Program&                ast,
                            Context&                           ctx,
                            llvm::legacy::FunctionPassManager& fp_manager)
{
  for (const auto& node : ast)
    boost::apply_visitor(TopLevelVisitor{ctx, fp_manager}, node);
}

void CodeGenerator::emitFiles(const llvm::CodeGenFileType cgft)
{
  static const std::unordered_map<llvm::CodeGenFileType, std::string>
    extension_map = {
      {llvm::CodeGenFileType::CGFT_AssemblyFile, "s"},
      {  llvm::CodeGenFileType::CGFT_ObjectFile, "o"}
  };

  for (auto it = results.begin(), last = results.end(); it != last; ++it) {
    const auto& file = std::get<std::filesystem::path>(*it);

    std::error_code      ostream_ec;
    llvm::raw_fd_ostream ostream{file.stem().string() + "."
                                   + extension_map.at(cgft),
                                 ostream_ec,
                                 llvm::sys::fs::OpenFlags::OF_None};

    if (ostream_ec) {
      throw std::runtime_error{format_error_message(
        argv_front,
        format("%s: %s\n", file.string(), ostream_ec.message()))};
    }

    llvm::legacy::PassManager p_manager;

    if (target_machine->addPassesToEmitFile(p_manager,
                                            ostream,
                                            nullptr,
                                            cgft)) {
      throw std::runtime_error{
        format_error_message(argv_front, "failed to emit a file", true)};
    }

    p_manager.run(*std::get<std::unique_ptr<llvm::Module>>(*it));
    ostream.flush();
  }
}

void CodeGenerator::initTargetTripleAndMachine()
{
  // Set target triple and data layout to module.
  target_triple = llvm::sys::getDefaultTargetTriple();

  std::string target_triple_error;
  auto const  target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_error);

  if (!target) {
    throw std::runtime_error{
      format_error_message(argv_front,
                           format("failed to lookup target %s: %s",
                                  target_triple,
                                  target_triple_error),
                           true)};
  }

  llvm::TargetOptions target_options;

  target_machine
    = target->createTargetMachine(target_triple,
                                  "generic",
                                  "",
                                  target_options,
                                  llvm::Optional<llvm::Reloc::Model>(
                                    relocation_model)); // Set relocation model.
}

} // namespace maple::codegen
