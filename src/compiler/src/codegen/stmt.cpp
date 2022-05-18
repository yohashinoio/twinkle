/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/codegen/stmt.hpp>
#include <maple/codegen/expr.hpp>
#include <maple/codegen/exception.hpp>

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

struct StmtVisitor : public boost::static_visitor<void> {
  StmtVisitor(CGContext&         ctx,
              SymbolTable&       scope,
              const StmtContext& stmt_ctx) noexcept
    : ctx{ctx}
    , scope{scope}
    , stmt_ctx{stmt_ctx}
  {
  }

  void operator()(ast::Nil) const
  {
    // Empty statement, so not processed.
  }

  void operator()(const ast::CompoundStmt& node) const
  {
    createStatement(ctx, scope, stmt_ctx, node);
  }

  void operator()(const ast::Expr& node) const
  {
    if (!createExpr(ctx, scope, stmt_ctx, node)) {
      throw CodegenError{
        formatError(ctx.file.string(),
                    "failed to generate expression statement")};
    }
  }

  void operator()(const ast::Return& node) const
  {
    if (node.rhs) {
      auto const retval = createExpr(ctx, scope, stmt_ctx, *node.rhs);

      auto const return_type
        = ctx.builder.GetInsertBlock()->getParent()->getReturnType();

      if (!strictEquals(return_type, retval.getType())) {
        throw CodegenError{
          ctx.formatError(ctx.positions.position_of(node),
                          "incompatible type for result type")};
      }

      if (!retval) {
        throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                           "failed to generate return value")};
      }

      ctx.builder.CreateStore(retval.getValue(), stmt_ctx.return_var);
    }

    ctx.builder.CreateBr(stmt_ctx.end_bb);
  }

  void operator()(const ast::VariableDef& node) const
  {
    if (!node.type.has_value() && !node.initializer.has_value()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "type inference requires an initializer")};
    }

    const auto name = node.name.utf8();

    auto const func = ctx.builder.GetInsertBlock()->getParent();

    const auto is_mutable
      = node.qualifier && (*node.qualifier == VariableQual::mutable_);

    if (node.type) {
      scope.registOrOverwrite(name,
                              createVariable(ctx.positions.position_of(node),
                                             func,
                                             name,
                                             **node.type,
                                             node.initializer,
                                             is_mutable));
    }
    else {
      scope.registOrOverwrite(
        name,
        createVariableTyInference(ctx.positions.position_of(node),
                                  func,
                                  name,
                                  node.initializer,
                                  is_mutable));
    }
  }

  void operator()(const ast::Assignment& node) const
  {
    const auto lhs
      = createAssignableValue(node.lhs, ctx.positions.position_of(node));

    auto const rhs = createExpr(ctx, scope, stmt_ctx, node.rhs);

    if (!rhs) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate right-hand side")};
    }

    if (!strictEquals(lhs.getType()->getPointerElementType(), rhs.getType())) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "both operands to a binary operator are not of the same type")};
    }

    auto const lhs_value
      = ctx.builder.CreateLoad(lhs.getType()->getPointerElementType(),
                               lhs.getValue());

    switch (node.kind()) {
    case ast::Assignment::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        fmt::format("unknown operator '{}' detected", node.operatorStr()))};

    case ast::Assignment::Kind::direct:
      ctx.builder.CreateStore(rhs.getValue(), lhs.getValue());
      return;

    case ast::Assignment::Kind::add:
      ctx.builder.CreateStore(ctx.builder.CreateAdd(lhs_value, rhs.getValue()),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::sub:
      ctx.builder.CreateStore(ctx.builder.CreateSub(lhs_value, rhs.getValue()),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::mul:
      ctx.builder.CreateStore(ctx.builder.CreateMul(lhs_value, rhs.getValue()),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::div:
      ctx.builder.CreateStore(
        isSigned(logicalOrSign(rhs, lhs))
          ? ctx.builder.CreateSDiv(lhs_value, rhs.getValue())
          : ctx.builder.CreateUDiv(lhs_value, rhs.getValue()),
        lhs.getValue());
      return;

    case ast::Assignment::Kind::mod:
      ctx.builder.CreateStore(
        isSigned(logicalOrSign(rhs, lhs))
          ? ctx.builder.CreateSRem(lhs_value, rhs.getValue())
          : ctx.builder.CreateURem(lhs_value, rhs.getValue()),
        lhs.getValue());
      return;
    }
  }

  void operator()(const ast::PrefixIncAndDec& node) const
  {
    const auto rhs
      = createAssignableValue(node.rhs, ctx.positions.position_of(node));

    auto const rhs_value
      = ctx.builder.CreateLoad(rhs.getType()->getPointerElementType(),
                               rhs.getValue());

    switch (node.kind()) {
    case ast::PrefixIncAndDec::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        fmt::format("unknown operator '{}' detected", node.operatorStr()))};

    case ast::PrefixIncAndDec::Kind::increment:
      ctx.builder.CreateStore(
        ctx.builder.CreateAdd(rhs_value,
                              llvm::ConstantInt::get(rhs_value->getType(), 1)),
        rhs.getValue());
      return;

    case ast::PrefixIncAndDec::Kind::decrement:
      ctx.builder.CreateStore(
        ctx.builder.CreateSub(rhs_value,
                              llvm::ConstantInt::get(rhs_value->getType(), 1)),
        rhs.getValue());
      return;
    }
  }

  void operator()(const ast::If& node) const
  {
    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const then_bb = llvm::BasicBlock::Create(ctx.context, "", func);
    auto const else_bb = llvm::BasicBlock::Create(ctx.context);

    auto const merge_bb = llvm::BasicBlock::Create(ctx.context);

    auto const cond_value = createExpr(ctx, scope, stmt_ctx, node.condition);
    if (!cond_value) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "invalid condition in if statement")};
    }

    if (!cond_value.getType()->isIntegerTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "condition type is incompatible with bool")};
    }

    // Compare with 0 and generate a condition of type booa.
    auto const cond
      = ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_NE,
                               cond_value.getValue(),
                               llvm::ConstantInt::get(cond_value.getType(), 0));

    ctx.builder.CreateCondBr(cond, then_bb, else_bb);

    // Then statement codegen.
    ctx.builder.SetInsertPoint(then_bb);

    createStatement(ctx, scope, stmt_ctx, node.then_statement);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(merge_bb);

    // Else statement codegen.
    func->getBasicBlockList().push_back(else_bb);
    ctx.builder.SetInsertPoint(else_bb);

    if (node.else_statement)
      createStatement(ctx, scope, stmt_ctx, *node.else_statement);

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

    createStatement(
      ctx,
      scope,
      {stmt_ctx.return_var, stmt_ctx.end_bb, loop_end_bb, body_bb},
      node.body);

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

    auto const cond_value = createExpr(ctx, scope, stmt_ctx, node.cond_expr);

    if (!cond_value) {
      throw CodegenError{
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

    createStatement(
      ctx,
      scope,
      {stmt_ctx.return_var, stmt_ctx.end_bb, loop_end_bb, cond_bb},
      node.body);

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

    const StmtContext new_stmt_ctx{stmt_ctx.return_var,
                                   stmt_ctx.end_bb,
                                   loop_end_bb,
                                   loop_bb};

    ctx.builder.CreateBr(cond_bb);
    ctx.builder.SetInsertPoint(cond_bb);

    if (node.cond_expr) {
      auto const cond_value
        = createExpr(ctx, scope, new_stmt_ctx, *node.cond_expr);

      if (!cond_value) {
        throw CodegenError{
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

    createStatement(ctx, scope, new_stmt_ctx, node.body);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(loop_bb);

    func->getBasicBlockList().push_back(loop_bb);
    ctx.builder.SetInsertPoint(loop_bb);

    // Generate loop statement.
    if (node.loop_stmt)
      createStatement(ctx, scope, new_stmt_ctx, *node.loop_stmt);

    ctx.builder.CreateBr(cond_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    ctx.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(ast::Break) const
  {
    if (stmt_ctx.break_bb) // If in a loop.
      ctx.builder.CreateBr(stmt_ctx.break_bb);
  }

  void operator()(ast::Continue) const
  {
    if (stmt_ctx.continue_bb) // If in a loop.
      ctx.builder.CreateBr(stmt_ctx.continue_bb);
  }

private:
  [[nodiscard]] Value
  createAssignableValue(const ast::Identifier&                     node,
                        const boost::iterator_range<InputIterator> pos) const
  {
    const auto& variable = findVariable(ctx, node, scope);

    if (!variable.isMutable()) {
      // Assignment to read-only variable.
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("assignment of read-only variable '{}'", node.utf8()))};
    }

    return {variable.getAllocaInst(),
            variable.getSignInfo(),
            variable.isMutable()};
  }

  [[nodiscard]] Value
  createAssignableValue(const ast::Subscript&                      node,
                        const boost::iterator_range<InputIterator> pos) const
  {
    const auto& variable = findVariable(ctx, node.ident, scope);

    if (!variable.isMutable()) {
      // Assignment of read-only variable.
      throw CodegenError{
        ctx.formatError(pos,
                        fmt::format("assignment of read-only variable '{}'",
                                    node.ident.utf8()))};
    }

    const auto tmp       = createExpr(ctx, scope, stmt_ctx, node);
    auto       tmp_stack = tmp.getSignInfo();
    tmp_stack.emplace(SignKind::unsigned_); // Pointer type.

    return {llvm::getPointerOperand(tmp.getValue()),
            std::move(tmp_stack),
            variable.isMutable()};
  }

  [[nodiscard]] Value
  createAssignableValue(const ast::Expr&                           node,
                        const boost::iterator_range<InputIterator> pos) const
  {
    Value value;

    if (node.type() == typeid(ast::Identifier))
      value = createAssignableValue(boost::get<ast::Identifier>(node), pos);
    else if (node.type() == typeid(ast::UnaryOp)
             && boost::get<ast::UnaryOp>(node).kind()
                  == ast::UnaryOp::Kind::indirection) {
      const auto& unary_op_node = boost::get<ast::UnaryOp>(node);

      value = createExpr(ctx, scope, stmt_ctx, unary_op_node.rhs);
    }
    else if (node.type() == typeid(ast::Subscript))
      value = createAssignableValue(boost::get<ast::Subscript>(node), pos);
    else
      value = createExpr(ctx, scope, stmt_ctx, node);

    if (!value)
      throw CodegenError{ctx.formatError(pos, "failed to generate expression")};

    if (!value.isMutable() || !value.isPointer()) {
      throw CodegenError{
        ctx.formatError(pos, "left-hand side value requires assignable")};
    }

    return value;
  }

  [[nodiscard]] InitializerList
  createInitializerList(const ast::InitializerList& initializer_list) const
  {
    InitializerList result;

    for (const auto& initializer : initializer_list.inits)
      result.emplace_back(createExpr(ctx, scope, stmt_ctx, initializer));

    return result;
  }

  void initArray(llvm::AllocaInst*      array_alloca,
                 const InitializerList& initializer_list) const
  {
    for (std::uint64_t i = 0, last = initializer_list.size(); i != last; ++i) {
      auto const gep = ctx.builder.CreateInBoundsGEP(
        array_alloca->getAllocatedType(),
        array_alloca,
        {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), 0),
         llvm::ConstantInt::get(ctx.builder.getInt64Ty(), i)});

      ctx.builder.CreateStore(initializer_list[i].getValue(), gep);
    }
  }

  [[nodiscard]] Variable
  createVariable(const boost::iterator_range<InputIterator>& pos,
                 llvm::Function*                             func,
                 const std::string&                          name,
                 const Type&                                 type,
                 const std::optional<ast::Initializer>&      initializer,
                 const bool                                  is_mutable) const
  {
    const auto variable_type = type.getType(ctx.context);
    auto const alloca        = createEntryAlloca(func, name, variable_type);

    if (!initializer) {
      return {
        {alloca, type.createSignKindStack()},
        is_mutable
      };
    }

    if (initializer->type() == typeid(ast::InitializerList)) {
      // Array type.
      if (!variable_type->isArrayTy()) {
        throw CodegenError{ctx.formatError(
          pos,
          "initializing an array requires an initializer list")};
      }

      const auto initializer_list
        = createInitializerList(boost::get<ast::InitializerList>(*initializer));

      if (type.getArraySize() != initializer_list.size()) {
        throw CodegenError{
          ctx.formatError(pos,
                          "invalid number of elements in initializer list")};
      }

      initArray(alloca, initializer_list);

      return {
        {alloca, type.createSignKindStack()},
        is_mutable
      };
    }
    else {
      // Primitive types.
      auto const init_value
        = createExpr(ctx, scope, stmt_ctx, boost::get<ast::Expr>(*initializer));

      if (!init_value) {
        throw CodegenError{ctx.formatError(
          pos,
          fmt::format("failed to generate initializer for '{}'", name))};
      }

      if (!strictEquals(variable_type, init_value.getType())) {
        throw CodegenError{ctx.formatError(
          pos,
          "the variable type and the initializer type are incompatible")};
      }

      if (variable_type->isIntegerTy()
          && variable_type->getIntegerBitWidth()
               != init_value.getType()->getIntegerBitWidth()) {
        throw CodegenError{
          ctx.formatError(pos, "different bit widths between operands")};
      }

      ctx.builder.CreateStore(init_value.getValue(), alloca);

      return {
        {alloca, type.createSignKindStack()},
        is_mutable
      };
    }

    unreachable();
  }

  [[nodiscard]] Variable
  createVariableTyInference(const boost::iterator_range<InputIterator>& pos,
                            llvm::Function*                             func,
                            const std::string&                          name,
                            const std::optional<ast::Initializer>& initializer,
                            const bool is_mutable) const
  {
    if (initializer->type() == typeid(ast::InitializerList)) {
      // Inference to array types.
      const auto initializer_list
        = createInitializerList(boost::get<ast::InitializerList>(*initializer));

      auto const type = initializer_list.getType();
      if (!type) {
        throw CodegenError{
          ctx.formatError(pos, "incompatibility type between initializers")};
      }

      auto const array_alloca = createEntryAlloca(
        func,
        name,
        llvm::ArrayType::get(type, initializer_list.size()));

      initArray(array_alloca, initializer_list);

      auto tmp = initializer_list.createFrontSignKindStack();
      tmp.emplace(SignKind::unsigned_); // Array type.
      return {
        {array_alloca, std::move(tmp)},
        is_mutable
      };
    }
    else {
      // Inference to primitive types.
      auto const init_value
        = createExpr(ctx, scope, stmt_ctx, boost::get<ast::Expr>(*initializer));

      if (!init_value) {
        throw CodegenError{ctx.formatError(
          pos,
          fmt::format("failed to generate initializer for '{}'", name))};
      }

      auto const alloca = createEntryAlloca(func, name, init_value.getType());

      ctx.builder.CreateStore(init_value.getValue(), alloca);

      return {
        {alloca, init_value.getSignInfo()},
        is_mutable
      };
    }

    unreachable();
  }

  CGContext& ctx;

  SymbolTable& scope;

  const StmtContext& stmt_ctx;
};

SymbolTable createStatement(CGContext&         ctx,
                            SymbolTable&       scope,
                            const StmtContext& stmt_ctx,
                            const ast::Stmt&   statement)
{
  auto new_scope = scope;

  if (statement.type() == typeid(ast::CompoundStmt)) {
    auto& statements = boost::get<ast::CompoundStmt>(statement);

    for (const auto& r : statements) {
      boost::apply_visitor(StmtVisitor{ctx, new_scope, stmt_ctx}, r);

      // Terminators cannot be placed in the middle of a basic block, so
      // inspect.
      if (ctx.builder.GetInsertBlock()->getTerminator())
        break;
    }
  }
  else
    boost::apply_visitor(StmtVisitor{ctx, new_scope, stmt_ctx}, statement);

  return new_scope;
}

} // namespace maple::codegen
