/**
 * stmt.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/stmt.hpp>
#include <codegen/expr.hpp>
#include <utils/format.hpp>

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

struct StmtVisitor : public boost::static_visitor<void> {
  StmtVisitor(CGContext&        ctx,
              SymbolTable&      scope,
              llvm::AllocaInst* retvar,
              llvm::BasicBlock* end_bb,
              llvm::BasicBlock* break_bb,
              llvm::BasicBlock* continue_bb) noexcept;

  void operator()(ast::Nil) const
  {
    // Empty statement, so not processed.
  }

  void operator()(const ast::CompoundStmt& node) const
  {
    genStmt(ctx, scope, node, retvar, end_bb, break_bb, continue_bb);
  }

  void operator()(const ast::Expr& node) const;

  void operator()(const ast::Return& node) const;

  void operator()(const ast::VariableDef& node) const;

  void operator()(const ast::Assignment& node) const;

  void operator()(const ast::PrefixIncAndDec& node) const;

  void operator()(const ast::If& node) const;

  void operator()(const ast::Loop& node) const;

  void operator()(const ast::While& node) const;

  void operator()(const ast::For& node) const;

  void operator()(ast::Break) const
  {
    if (break_bb) // If in a loop.
      ctx.builder.CreateBr(break_bb);
  }

  void operator()(ast::Continue) const
  {
    if (continue_bb) // If in a loop.
      ctx.builder.CreateBr(continue_bb);
  }

private:
  [[nodiscard]] Value genAssignableValueFromExpr(
    const ast::Expr&                                  node,
    const boost::iterator_range<maple::InputIterator> pos) const;

  [[nodiscard]] std::vector<llvm::Value*>
  genInitList(const ast::InitList& list) const;

  void InitArray(llvm::AllocaInst*                array_alloca,
                 const std::vector<llvm::Value*>& init_list) const;

  [[nodiscard]] llvm::AllocaInst* createVariableWithType(
    const boost::iterator_range<InputIterator>& pos,
    llvm::Function*                             func,
    const std::string&                          name,
    const Type&                                 type,
    const std::optional<ast::Initializer>&      initializer) const;

  [[nodiscard]] std::pair<llvm::AllocaInst*, bool /* Is signed */>
  createVariableWithTypeInference(
    const boost::iterator_range<InputIterator>& pos,
    llvm::Function*                             func,
    const std::string&                          name,
    const std::optional<ast::Initializer>&      initializer) const;

  CGContext& ctx;

  SymbolTable& scope;

  // Used to combine returns into one.
  llvm::AllocaInst* retvar;
  llvm::BasicBlock* end_bb;

  // If not in loop, nullptr.
  llvm::BasicBlock* break_bb; // Basic block transitioned by break statement.
  llvm::BasicBlock*
    continue_bb; // Basic block transitioned by continue statement.
};

StmtVisitor::StmtVisitor(CGContext&        ctx,
                         SymbolTable&      scope,
                         llvm::AllocaInst* retvar,
                         llvm::BasicBlock* end_bb,
                         llvm::BasicBlock* break_bb,
                         llvm::BasicBlock* continue_bb) noexcept
  : ctx{ctx}
  , scope{scope}
  , retvar{retvar}
  , end_bb{end_bb}
  , break_bb{break_bb}
  , continue_bb{continue_bb}
{
}

void StmtVisitor::operator()(const ast::Expr& node) const
{
  if (!genExpr(ctx, scope, node)) {
    throw std::runtime_error{
      formatErrorMessage(ctx.file.string(),
                           "failed to generate expression statement")};
  }
}

void StmtVisitor::operator()(const ast::Return& node) const
{
  if (node.rhs) {
    auto const retval = genExpr(ctx, scope, *node.rhs);

    if (ctx.builder.GetInsertBlock()->getParent()->getReturnType()
        != retval.getType()) {
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

void StmtVisitor::operator()(const ast::VariableDef& node) const
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
          // Constant variable.
          scope.regist(node.name, {alloca, false, is_signed});
        }
        else if (*node.qualifier == VariableQual::mutable_) {
          // Mutable variable.
          scope.regist(node.name, {alloca, true, is_signed});
        }
      };

  if (node.type) {
    const auto& type = **node.type;
    regist(createVariableWithType(ctx.positions.position_of(node),
                                  func,
                                  node.name,
                                  type,
                                  node.initializer),
           type.isSigned());
  }
  else {
    auto [alloca, is_signed]
      = createVariableWithTypeInference(ctx.positions.position_of(node),
                                        func,
                                        node.name,
                                        node.initializer);
    regist(alloca, is_signed);
  }
}

void StmtVisitor::operator()(const ast::Assignment& node) const
{
  if (node.op == "=" || node.op == "+=" || node.op == "-=" || node.op == "*="
      || node.op == "/=" || node.op == "%=") {
    const auto lhs
      = genAssignableValueFromExpr(node.lhs, ctx.positions.position_of(node));

    auto const rhs = genExpr(ctx, scope, node.rhs);

    if (!rhs) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate right-hand side")};
    }

    if (lhs.getType()->getPointerElementType() != rhs.getType()) {
      throw std::runtime_error{ctx.formatError(
        ctx.positions.position_of(node),
        "both operands to a binary operator are not of the same type")};
    }

    auto const lhs_value
      = ctx.builder.CreateLoad(lhs.getType()->getPointerElementType(),
                               lhs.getValue());

    // Direct assignment.
    if (node.op == "=") // TODO: to isXXX()
      ctx.builder.CreateStore(rhs.getValue(), lhs.getValue());

    // Addition assignment.
    if (node.op == "+=") {
      ctx.builder.CreateStore(ctx.builder.CreateAdd(lhs_value, rhs.getValue()),
                              lhs.getValue());
    }

    // Subtraction assignment.
    if (node.op == "-=") {
      ctx.builder.CreateStore(ctx.builder.CreateSub(lhs_value, rhs.getValue()),
                              lhs.getValue());
    }

    // Multiplication assignment.
    if (node.op == "*=") {
      ctx.builder.CreateStore(ctx.builder.CreateMul(lhs_value, rhs.getValue()),
                              lhs.getValue());
    }

    const auto result_is_signed
      = rhs.isSigned() || lhs.isSigned() ? true : false;

    // Division assignment.
    if (node.op == "/=") {
      auto const assign_value
        = result_is_signed ? ctx.builder.CreateSDiv(lhs_value, rhs.getValue())
                           : ctx.builder.CreateUDiv(lhs_value, rhs.getValue());

      ctx.builder.CreateStore(assign_value, lhs.getValue());
    }

    // Modulo assignment.
    if (node.op == "%=") {
      auto const assign_value
        = result_is_signed ? ctx.builder.CreateSRem(lhs_value, rhs.getValue())
                           : ctx.builder.CreateURem(lhs_value, rhs.getValue());

      ctx.builder.CreateStore(assign_value, lhs.getValue());
    }
  }
}

void StmtVisitor::operator()(const ast::PrefixIncAndDec& node) const
{
  const auto rhs
    = genAssignableValueFromExpr(node.rhs, ctx.positions.position_of(node));

  auto const rhs_value
    = ctx.builder.CreateLoad(rhs.getType()->getPointerElementType(),
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

void StmtVisitor::operator()(const ast::If& node) const
{
  auto const cond_value = genExpr(ctx, scope, node.condition);

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
      BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.context),
      0));

  auto const func = ctx.builder.GetInsertBlock()->getParent();

  auto const then_bb = llvm::BasicBlock::Create(ctx.context, "", func);
  auto const else_bb = llvm::BasicBlock::Create(ctx.context);

  auto const merge_bb = llvm::BasicBlock::Create(ctx.context);

  ctx.builder.CreateCondBr(cond, then_bb, else_bb);

  // Then statement codegen.
  ctx.builder.SetInsertPoint(then_bb);

  genStmt(ctx,
          scope,
          node.then_statement,
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
    genStmt(ctx,
            scope,
            *node.else_statement,
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

void StmtVisitor::operator()(const ast::Loop& node) const
{
  auto const func = ctx.builder.GetInsertBlock()->getParent();

  auto const body_bb = llvm::BasicBlock::Create(ctx.context, "", func);

  auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context);

  ctx.builder.CreateBr(body_bb);
  ctx.builder.SetInsertPoint(body_bb);

  genStmt(ctx, scope, node.body, retvar, end_bb, loop_end_bb, body_bb);

  if (!ctx.builder.GetInsertBlock()->getTerminator())
    ctx.builder.CreateBr(body_bb);

  func->getBasicBlockList().push_back(loop_end_bb);
  ctx.builder.SetInsertPoint(loop_end_bb);
}

void StmtVisitor::operator()(const ast::While& node) const
{
  auto const func = ctx.builder.GetInsertBlock()->getParent();

  auto const cond_bb = llvm::BasicBlock::Create(ctx.context, "", func);
  auto const body_bb = llvm::BasicBlock::Create(ctx.context);

  auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context);

  ctx.builder.CreateBr(cond_bb);
  ctx.builder.SetInsertPoint(cond_bb);

  auto const cond_value = genExpr(ctx, scope, node.cond_expr);

  if (!cond_value) {
    throw std::runtime_error{
      ctx.formatError(ctx.positions.position_of(node),
                      "failed to generate condition expression")};
  }

  auto const cond = ctx.builder.CreateICmp(
    llvm::ICmpInst::ICMP_NE,
    cond_value.getValue(),
    llvm::ConstantInt::get(
      BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.context),
      0));

  ctx.builder.CreateCondBr(cond, body_bb, loop_end_bb);

  func->getBasicBlockList().push_back(body_bb);
  ctx.builder.SetInsertPoint(body_bb);

  genStmt(ctx, scope, node.body, retvar, end_bb, loop_end_bb, cond_bb);

  if (!ctx.builder.GetInsertBlock()->getTerminator())
    ctx.builder.CreateBr(cond_bb);

  func->getBasicBlockList().push_back(loop_end_bb);
  ctx.builder.SetInsertPoint(loop_end_bb);
}

void StmtVisitor::operator()(const ast::For& node) const
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
    auto const cond_value = genExpr(ctx, scope, *node.cond_expr);

    if (!cond_value) {
      throw std::runtime_error{
        ctx.formatError(ctx.positions.position_of(node),
                        "failed to generate condition expression")};
    }

    auto const cond = ctx.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value.getValue(),
      llvm::ConstantInt::get(
        BuiltinType{BuiltinTypeKind::bool_}.getType(ctx.context),
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

  genStmt(ctx, scope, node.body, retvar, end_bb, loop_end_bb, loop_bb);

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

[[nodiscard]] Value StmtVisitor::genAssignableValueFromExpr(
  const ast::Expr&                                  node,
  const boost::iterator_range<maple::InputIterator> pos) const
{
  Value value;

  if (node.type() == typeid(ast::Identifier)) {
    const auto& identifier = boost::get<ast::Identifier>(node).name;

    auto variable = scope[identifier];

    if (!variable) {
      // Unknown variable name.
      throw std::runtime_error{
        ctx.formatError(pos, format("unknown variable name '%s'", identifier))};
    }

    if (!variable->isMutable()) {
      // Assignment of read-only variable.
      throw std::runtime_error{ctx.formatError(
        pos,
        format("assignment of read-only variable '%s'", identifier))};
    }

    value = {variable->getAllocaInst(), variable->isSigned()};
  }
  else if (node.type() == typeid(ast::UnaryOp)
           && boost::get<ast::UnaryOp>(node).isIndirection()) {
    const auto& unary_op_node = boost::get<ast::UnaryOp>(node);

    value = genExpr(ctx, scope, unary_op_node.rhs);
  }
  else
    value = genExpr(ctx, scope, node);

  if (!value) {
    throw std::runtime_error{
      ctx.formatError(pos, "failed to generate left-hand side")};
  }

  if (!value.getType()->isPointerTy()) {
    throw std::runtime_error{
      ctx.formatError(pos, "left-hand side requires assignable")};
  }

  return value;
}

[[nodiscard]] std::vector<llvm::Value*>
StmtVisitor::genInitList(const ast::InitList& list) const
{
  std::vector<llvm::Value*> result;

  for (auto init : list.inits)
    result.push_back(genExpr(ctx, scope, init).getValue());

  return result;
}

void StmtVisitor::InitArray(llvm::AllocaInst*                array_alloca,
                            const std::vector<llvm::Value*>& init_list) const
{
  for (unsigned int i = 0, last = init_list.size(); i != last; ++i) {
    ctx.builder.CreateInsertValue(
      ctx.builder.CreateLoad(array_alloca->getAllocatedType(), array_alloca),
      init_list[i],
      {i});
  }
}

[[nodiscard]] llvm::AllocaInst* StmtVisitor::createVariableWithType(
  const boost::iterator_range<InputIterator>& pos,
  llvm::Function*                             func,
  const std::string&                          name,
  const Type&                                 type,
  const std::optional<ast::Initializer>&      initializer) const
{
  const auto llvm_type = type.getType(ctx.context);
  auto const alloca    = createEntryAlloca(func, name, llvm_type);

  if (!initializer)
    return alloca;

  if (initializer->type() == typeid(ast::InitList)) {
    // Array initialization.
    if (!llvm_type->isArrayTy()) {
      throw std::runtime_error{
        ctx.formatError(pos,
                        "initializing an array requires an initializer list")};
    }

    const auto init_list = genInitList(boost::get<ast::InitList>(*initializer));

    if (type.getArraySize() != init_list.size()) {
      throw std::runtime_error{
        ctx.formatError(pos, "invalid number of elements in initializer list")};
    }

    InitArray(alloca, init_list);
  }
  else {
    // Normal initialization.
    auto const init_value
      = genExpr(ctx, scope, boost::get<ast::Expr>(*initializer));

    if (!init_value) {
      throw std::runtime_error{ctx.formatError(
        pos,
        format("failed to generate initializer for '%s'", name))};
    }

    if (!equals(llvm_type, init_value.getType())) {
      throw std::runtime_error{ctx.formatError(
        pos,
        "the variable type and the initializer type are incompatible")};
    }

    if (llvm_type->isIntegerTy()
        && llvm_type->getIntegerBitWidth()
             != init_value.getType()->getIntegerBitWidth()) {
      throw std::runtime_error{
        ctx.formatError(pos, "different bit widths between operands")};
    }

    ctx.builder.CreateStore(init_value.getValue(), alloca);
  }

  return alloca;
}

[[nodiscard]] std::pair<llvm::AllocaInst*, bool /* is signed */>
StmtVisitor::createVariableWithTypeInference(
  const boost::iterator_range<InputIterator>& pos,
  llvm::Function*                             func,
  const std::string&                          name,
  const std::optional<ast::Initializer>&      initializer) const
{
  if (initializer->type() == typeid(ast::InitList)) {
    // Guess to array.
    const auto init_list = genInitList(boost::get<ast::InitList>(*initializer));

    auto const array_alloca = createEntryAlloca(
      func,
      name,
      llvm::ArrayType::get(init_list.front()->getType(), init_list.size()));

    InitArray(array_alloca, init_list);

    return {array_alloca, false};
  }
  else {
    auto const init_value
      = genExpr(ctx, scope, boost::get<ast::Expr>(*initializer));

    if (!init_value) {
      throw std::runtime_error{ctx.formatError(
        pos,
        format("failed to generate initializer for '%s'", name))};
    }

    auto const alloca = createEntryAlloca(func, name, init_value.getType());

    ctx.builder.CreateStore(init_value.getValue(), alloca);

    return {alloca, init_value.isSigned()};
  }

  unreachable();
}

void genStmt(CGContext&        ctx,
             SymbolTable&      scope,
             const ast::Stmt&  statement,
             llvm::AllocaInst* retvar,
             llvm::BasicBlock* end_bb,
             llvm::BasicBlock* break_bb,
             llvm::BasicBlock* continue_bb)
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

} // namespace maple::codegen
