/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/codegen/stmt.hpp>
#include <twinkle/codegen/expr.hpp>
#include <twinkle/codegen/exception.hpp>

namespace twinkle::codegen
{

[[nodiscard]] SymbolTable mergeSymbolTables(const SymbolTable& a,
                                            const SymbolTable& b)
{
  SymbolTable merged_table{a};

  for (const auto& r : b) {
    // May be shadowed and must be overwriteable
    merged_table.insertOrAssign(r.first, r.second);
  }

  return merged_table;
}

//===----------------------------------------------------------------------===//
// Statement visitor
//===----------------------------------------------------------------------===//

struct StmtVisitor : public boost::static_visitor<void> {
  StmtVisitor(CGContext&         ctx,
              const SymbolTable& parent_scope,
              SymbolTable&       scope,
              const StmtContext& stmt_ctx) noexcept
    : ctx{ctx}
    , parent_scope{parent_scope}
    , scope{scope}
    , stmt_ctx{stmt_ctx}
  {
  }

  // Empty statement
  void operator()(boost::blank) const
  {
  }

  void operator()(const ast::CompoundStatement& node) const
  {
    createStatement(ctx, getAllSymbols(), stmt_ctx, node);
  }

  void operator()(const ast::Expr& node) const
  {
    static_cast<void>(createExpr(ctx, getAllSymbols(), stmt_ctx, node));
  }

  void operator()(const ast::Return& node) const
  {
    if (node.rhs) {
      auto const retval = createExpr(ctx, getAllSymbols(), stmt_ctx, *node.rhs);

      auto const return_type
        = ctx.return_type_table[ctx.builder.GetInsertBlock()->getParent()];

      assert(return_type);

      if (!equals(ctx, *return_type, retval.getType())) {
        throw CodegenError{
          ctx.formatError(ctx.positionOf(node),
                          "incompatible type for result type")};
      }

      ctx.builder.CreateStore(retval.getValue(), stmt_ctx.return_var);
    }

    ctx.builder.CreateBr(stmt_ctx.destruct_bb);
  }

  void operator()(const ast::VariableDef& node) const
  {
    if (!node.type && !node.initializer) {
      throw CodegenError{
        ctx.formatError(ctx.positionOf(node),
                        "type inference requires an initializer")};
    }

    const auto name = node.name.utf8();

    auto const func = ctx.builder.GetInsertBlock()->getParent();

    const auto is_mutable
      = node.qualifier && (*node.qualifier == VariableQual::mutable_);

    if (node.type) {
      const auto type = createType(ctx, *node.type, ctx.positionOf(node));

      scope.insertOrAssign(name,
                           std::make_shared<AllocaVariable>(
                             createAllocaVariable(ctx.positionOf(node),
                                                  func,
                                                  name,
                                                  type,
                                                  node.initializer,
                                                  is_mutable)));
    }
    else {
      scope.insertOrAssign(
        name,
        std::make_shared<AllocaVariable>(
          createAllocaVariableTyInference(ctx.positionOf(node),
                                          func,
                                          name,
                                          node.initializer,
                                          is_mutable)));
    }
  }

  void operator()(const ast::Assignment& node) const
  {
    return createAssignment(node);
  }

  void operator()(const ast::ClassMemberInit& node) const
  {
    return createAssignment(node, false);
  }

  void operator()(const ast::PrefixIncrementDecrement& node) const
  {
    const auto pos = ctx.positionOf(node);

    const auto operand
      = createAssignableValue(node.operand, ctx.positionOf(node));

    const auto derefed_operand = createDereference(ctx, pos, operand);

    const auto one
      = Value{llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 1),
              std::make_shared<BuiltinType>(BuiltinTypeKind::i32, false)};

    switch (node.kind()) {
    case ast::PrefixIncrementDecrement::Kind::unknown:
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("unknown operator '{}' detected", node.opstr()))};

    case ast::PrefixIncrementDecrement::Kind::increment:
      ctx.builder.CreateStore(createAdd(ctx, derefed_operand, one).getValue(),
                              operand.getValue());
      return;

    case ast::PrefixIncrementDecrement::Kind::decrement:
      ctx.builder.CreateStore(
        // If lhs and rhs are reversed, it will not work correctly
        createSub(ctx, derefed_operand, one).getValue(),
        operand.getValue());
      return;
    }
  }

  void operator()(const ast::If& node) const
  {
    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const then_bb = llvm::BasicBlock::Create(ctx.context, "if_then", func);
    auto const else_bb = llvm::BasicBlock::Create(ctx.context, "if_else");

    auto const merge_bb = llvm::BasicBlock::Create(ctx.context, "if_merge");

    auto const cond_value
      = createExpr(ctx, getAllSymbols(), stmt_ctx, node.condition);

    if (!cond_value.getLLVMType()->isIntegerTy()
        && !cond_value.getLLVMType()->isPointerTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positionOf(node),
                        "condition type is incompatible with bool")};
    }

    auto const cond = ctx.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      cond_value.getValue(),
      llvm::Constant::getNullValue(cond_value.getLLVMType()));

    ctx.builder.CreateCondBr(cond, then_bb, else_bb);

    // Then statement codegen
    ctx.builder.SetInsertPoint(then_bb);

    createStatement(ctx, getAllSymbols(), stmt_ctx, node.then_statement);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(merge_bb);

    // Else statement codegen
    func->getBasicBlockList().push_back(else_bb);
    ctx.builder.SetInsertPoint(else_bb);

    if (node.else_statement)
      createStatement(ctx, getAllSymbols(), stmt_ctx, *node.else_statement);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(merge_bb);

    func->getBasicBlockList().push_back(merge_bb);

    ctx.builder.SetInsertPoint(merge_bb);
  }

  void operator()(const ast::Loop& node) const
  {
    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const body_bb
      = llvm::BasicBlock::Create(ctx.context, "loop_body", func);

    auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context, "loop_end");

    ctx.builder.CreateBr(body_bb);
    ctx.builder.SetInsertPoint(body_bb);

    createStatement(ctx,
                    getAllSymbols(),
                    {stmt_ctx.destruct_bb,
                     stmt_ctx.return_var,
                     stmt_ctx.end_bb,
                     loop_end_bb,
                     body_bb},
                    node.body);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(body_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    ctx.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(const ast::While& node) const
  {
    auto const func = ctx.builder.GetInsertBlock()->getParent();

    auto const cond_bb
      = llvm::BasicBlock::Create(ctx.context, "while_cond", func);
    auto const body_bb = llvm::BasicBlock::Create(ctx.context, "while_body");

    auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context, "while_end");

    ctx.builder.CreateBr(cond_bb);
    ctx.builder.SetInsertPoint(cond_bb);

    auto const cond = ctx.builder.CreateICmp(
      llvm::ICmpInst::ICMP_NE,
      createExpr(ctx, getAllSymbols(), stmt_ctx, node.cond_expr).getValue(),
      llvm::ConstantInt::get(
        BuiltinType{BuiltinTypeKind::bool_, false}.getLLVMType(ctx),
        0));

    ctx.builder.CreateCondBr(cond, body_bb, loop_end_bb);

    func->getBasicBlockList().push_back(body_bb);
    ctx.builder.SetInsertPoint(body_bb);

    createStatement(ctx,
                    getAllSymbols(),
                    {stmt_ctx.destruct_bb,
                     stmt_ctx.return_var,
                     stmt_ctx.end_bb,
                     loop_end_bb,
                     cond_bb},
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

    auto const cond_bb
      = llvm::BasicBlock::Create(ctx.context, "for_cond", func);
    auto const loop_bb = llvm::BasicBlock::Create(ctx.context, "for_loop");
    auto const body_bb = llvm::BasicBlock::Create(ctx.context, "for_body");

    auto const loop_end_bb = llvm::BasicBlock::Create(ctx.context, "for_end");

    const StmtContext new_stmt_ctx{stmt_ctx.destruct_bb,
                                   stmt_ctx.return_var,
                                   stmt_ctx.end_bb,
                                   loop_end_bb,
                                   loop_bb};

    ctx.builder.CreateBr(cond_bb);
    ctx.builder.SetInsertPoint(cond_bb);

    if (node.cond_expr) {
      auto const cond = ctx.builder.CreateICmp(
        llvm::ICmpInst::ICMP_NE,
        createExpr(ctx, getAllSymbols(), new_stmt_ctx, *node.cond_expr)
          .getValue(),
        llvm::ConstantInt::get(
          BuiltinType{BuiltinTypeKind::bool_, false}.getLLVMType(ctx),
          0));

      ctx.builder.CreateCondBr(cond, body_bb, loop_end_bb);
    }
    else {
      // If condition is absent, unconditionally true
      ctx.builder.CreateCondBr(
        llvm::ConstantInt::get(ctx.builder.getInt1Ty(), true),
        body_bb,
        loop_end_bb);
    }

    func->getBasicBlockList().push_back(body_bb);
    ctx.builder.SetInsertPoint(body_bb);

    createStatement(ctx, getAllSymbols(), new_stmt_ctx, node.body);

    if (!ctx.builder.GetInsertBlock()->getTerminator())
      ctx.builder.CreateBr(loop_bb);

    func->getBasicBlockList().push_back(loop_bb);
    ctx.builder.SetInsertPoint(loop_bb);

    // Generate loop statement
    if (node.loop_stmt)
      createStatement(ctx, getAllSymbols(), new_stmt_ctx, *node.loop_stmt);

    ctx.builder.CreateBr(cond_bb);

    func->getBasicBlockList().push_back(loop_end_bb);
    ctx.builder.SetInsertPoint(loop_end_bb);
  }

  void operator()(ast::Break) const
  {
    if (stmt_ctx.break_bb) // If in a loop
      ctx.builder.CreateBr(stmt_ctx.break_bb);
  }

  void operator()(ast::Continue) const
  {
    if (stmt_ctx.continue_bb) // If in a loop
      ctx.builder.CreateBr(stmt_ctx.continue_bb);
  }

  void operator()(const ast::Match& node) const
  {
    const auto target_val
      = createExpr(ctx, getAllSymbols(), stmt_ctx, node.target);

    const auto target_type = target_val.getType();

    if (target_type->isUnionTy(ctx)) {
      createUnionMatch(target_val, node.cases, ctx.positionOf(node));
      return;
    }
    else {
      throw CodegenError{
        ctx.formatError(ctx.positionOf(node),
                        "a target that does not support match is specified")};
    }

    unreachable();
  }

private:
  // Create a match statement targeting union
  void createUnionMatch(const Value&                       target,
                        const std::vector<ast::MatchCase>& cases,
                        const PositionRange&               pos) const
  {
    const auto target_ty = target.getType();
    assert(target_ty->isUnionTy(ctx));

    const auto tag_offset = getUnionTagOffsetFromValue(target);

    ast::Stmt                cases_ast;
    ast::Stmt*               before = nullptr;
    std::optional<ast::Stmt> wildcard_statement;

    for (const auto& cs /* case */ : cases) {
      // Wildcard
      if (isWildcard(cs.match_case)) {
        wildcard_statement = cs.statement;
        continue;
      }

      // Union tag
      if (auto const union_tag
          = boost::get<ast::ScopeResolution>(&cs.match_case)) {
        const auto offset
          = getUnionTagOffset(target_ty->getUnionVariants(ctx), *union_tag);

        if (!offset.first) {
          throw CodegenError{ctx.formatError(
            ctx.positionOf(*union_tag),
            fmt::format("undeclared union tag name {}", offset.second))};
        }

        ast::BinOp eq{ast::Value{&tag_offset}, U"==", *offset.first};
        assignPosition(eq, cs);
        ast::If ast{std::move(eq), cs.statement, std::nullopt};
        assignPosition(ast, cs);

        if (before) {
          auto const tmp      = boost::get<ast::If>(before);
          tmp->else_statement = std::move(ast);
          before              = &*tmp->else_statement;
        }
        else {
          // first time
          cases_ast = std::move(ast);
          before    = &cases_ast;
        }

        continue;
      }
      else {
        throw CodegenError{ctx.formatError(
          pos,
          "case of match targeting a union must be a union tag")};
      }

      unreachable();
    }

    // Insert wildcard
    if (wildcard_statement) {
      auto const tmp      = boost::get<ast::If>(before);
      tmp->else_statement = std::move(*wildcard_statement);
    }

    // Generate cases
    (*this)(boost::get<ast::If>(cases_ast));
  }

  [[nodiscard]] bool isWildcard(const ast::Expr& node) const
  {
    if (auto const ident = boost::get<ast::Identifier>(&node)) {
      if (ident->utf32() == U"_")
        return true;
    }

    return false;
  }

  [[nodiscard]] std::pair<std::optional<std::uint8_t> /* offset */,
                          std::string /* union tag name */>
  getUnionTagOffset(const UnionVariants&        variants,
                    const ast::ScopeResolution& node) const
  {
    const auto result = createScopeResolutionResult(ctx, node);

    if (auto const tag_name = boost::get<ast::Identifier>(&result.expr)) {
      const auto tag_name_str = tag_name->utf8();

      for (const auto& variant : variants) {
        if (variant.tag == tag_name_str)
          return {variant.offset, std::move(tag_name_str)};
      }

      return {std::nullopt, std::move(tag_name_str)};
    }
    else {
      throw CodegenError{
        ctx.formatError(ctx.positionOf(node),
                        "rightmost side must be an identifier")};
    }

    unreachable();
  }

  // Returns the first element of a union (tag offset)
  [[nodiscard]] Value getUnionTagOffsetFromValue(const Value& union_) const
  {
    const auto ty = union_.getType();

    assert(ty->isUnionTy(ctx));

    const auto value = gepByOffset(ctx,
                                   llvm::getPointerOperand(union_.getValue()),
                                   union_.getLLVMType(),
                                   0);

    return {
      ctx.builder.CreateLoad(value->getType()->getPointerElementType(), value),
      std::make_shared<BuiltinType>(BuiltinTypeKind::u8, false)};
  }

  [[nodiscard]] SymbolTable getAllSymbols() const
  {
    return mergeSymbolTables(parent_scope, scope);
  }

  void createAssignment(const ast::Assignment& node,
                        const bool             const_check = true) const
  {
    const auto lhs
      = createAssignableValue(node.lhs, ctx.positionOf(node), const_check);

    const auto rhs = createExpr(ctx, getAllSymbols(), stmt_ctx, node.rhs);

    verifyVariableType(ctx.positionOf(node), rhs.getType());

    auto const lhs_value
      = Value{ctx.builder.CreateLoad(lhs.getLLVMType()->getPointerElementType(),
                                     lhs.getValue()),
              lhs.getType()->getPointeeType(ctx)};

    switch (node.kind()) {
    case ast::Assignment::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positionOf(node),
        fmt::format("unknown operator '{}' detected", node.opstr()))};

    case ast::Assignment::Kind::direct:
      ctx.builder.CreateStore(rhs.getValue(), lhs.getValue());
      return;

    case ast::Assignment::Kind::add:
      ctx.builder.CreateStore(createAdd(ctx, lhs_value, rhs).getValue(),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::sub:
      ctx.builder.CreateStore(createSub(ctx, lhs_value, rhs).getValue(),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::mul:
      ctx.builder.CreateStore(createMul(ctx, lhs_value, rhs).getValue(),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::div:
      ctx.builder.CreateStore(createDiv(ctx, lhs_value, rhs).getValue(),
                              lhs.getValue());
      return;

    case ast::Assignment::Kind::mod:
      ctx.builder.CreateStore(createMod(ctx, lhs_value, rhs).getValue(),
                              lhs.getValue());
      return;
    }
  }

  [[nodiscard]] Value createAssignableValue(const ast::Expr&     node,
                                            const PositionRange& pos,
                                            const bool const_check = true) const
  {
    const auto value = createExpr(ctx, getAllSymbols(), stmt_ctx, node);

    if (const_check && !value.isMutable()) {
      throw CodegenError{
        ctx.formatError(pos, "assignment of read-only variable")};
    }

    if (value.getType()->isRefTy(ctx)) {
      // Since reference types wrap pointer types
      return value;
    }

    return {llvm::getPointerOperand(value.getValue()),
            std::make_shared<PointerType>(value.getType(), value.isMutable())};
  }

  void verifyVariableType(const PositionRange&         pos,
                          const std::shared_ptr<Type>& type) const
  {
    if (type->isVoidTy(ctx)) {
      throw CodegenError{
        ctx.formatError(pos, "variable has incomplete type 'void'")};
    }
  }

  [[nodiscard]] AllocaVariable
  createAllocaVariable(const PositionRange&            pos,
                       llvm::Function*                 func,
                       const std::string&              name,
                       const std::shared_ptr<Type>&    type,
                       const std::optional<ast::Expr>& initializer,
                       const bool                      is_mutable) const
  {
    verifyVariableType(pos, type);

    auto const alloca = createEntryAlloca(func, name, type->getLLVMType(ctx));

    if (!initializer) {
      return {
        ctx,
        {alloca, type->clone()},
        is_mutable
      };
    }

    auto const init_value
      = createExpr(ctx, getAllSymbols(), stmt_ctx, *initializer);

    if (!equals(ctx, type, init_value.getType()))
      throw CodegenError{ctx.formatError(pos, "invalid initializer type")};

    ctx.builder.CreateStore(init_value.getValue(), alloca);

    return {
      ctx,
      {alloca, type->clone()},
      is_mutable
    };
  }

  [[nodiscard]] AllocaVariable
  createAllocaVariableTyInference(const PositionRange&            pos,
                                  llvm::Function*                 func,
                                  const std::string&              name,
                                  const std::optional<ast::Expr>& initializer,
                                  const bool is_mutable) const
  {
    auto const init_value
      = createExpr(ctx, getAllSymbols(), stmt_ctx, *initializer);

    verifyVariableType(pos, init_value.getType());

    auto const alloca = createEntryAlloca(func, name, init_value.getLLVMType());

    ctx.builder.CreateStore(init_value.getValue(), alloca);

    return {
      ctx,
      {alloca, init_value.getType()->clone()},
      is_mutable
    };
  }

  CGContext& ctx;

  const SymbolTable& parent_scope;

  SymbolTable& scope;

  const StmtContext& stmt_ctx;
};

[[nodiscard]] llvm::Function* findDestructor(CGContext&         ctx,
                                             const std::string& class_name)
{
  ctx.ns_hierarchy.push({class_name, NamespaceKind::class_});

  const auto destructor
    = findFunction(ctx, ctx.mangler.mangleDestructorCall(class_name));

  ctx.ns_hierarchy.pop();

  return destructor;
}

// If destructor is not defined, nothing is done
void invokeDestructor(CGContext& ctx, const Value& this_)
{
  assert(this_.getType()->isClassTy(ctx));

  const auto destructor
    = findDestructor(ctx, this_.getType()->getClassName(ctx));

  if (destructor) {
    ctx.builder.CreateCall(destructor,
                           {llvm::getPointerOperand(this_.getValue())});
  }
}

// If destructor is not defined, nothing is done
void invokeDestructor(CGContext& ctx, const std::shared_ptr<Variable>& this_)
{
  assert(this_->getType()->isClassTy(ctx));

  const auto destructor
    = findDestructor(ctx, this_->getType()->getClassName(ctx));

  if (destructor)
    ctx.builder.CreateCall(destructor, {this_->getAllocaInst()});
}

static void createDestructBB(CGContext&         ctx,
                             const StmtContext& stmt_ctx,
                             const SymbolTable& symbols,
                             const bool         return_)
{
  ctx.builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(
    stmt_ctx.destruct_bb);
  ctx.builder.SetInsertPoint(stmt_ctx.destruct_bb);

  for (const auto& symbol : symbols) {
    if (symbol.second->getType()->isClassTy(ctx))
      invokeDestructor(ctx, symbol.second);
  }

  if (return_)
    ctx.builder.CreateBr(stmt_ctx.end_bb);
}

void createStatement(CGContext&         ctx,
                     const SymbolTable& scope_arg,
                     const StmtContext& stmt_ctx_arg,
                     const ast::Stmt&   statement)
{
  SymbolTable new_scope;

  auto new_stmt_ctx        = stmt_ctx_arg;
  new_stmt_ctx.destruct_bb = llvm::BasicBlock::Create(ctx.context, "destruct");

  if (statement.type() == typeid(ast::CompoundStatement)) {
    auto& statements = boost::get<ast::CompoundStatement>(statement);

    for (const auto& r : statements) {
      boost::apply_visitor(StmtVisitor{ctx, scope_arg, new_scope, new_stmt_ctx},
                           r);

      if (ctx.builder.GetInsertBlock()->getTerminator()) {
        // Terminators cannot be placed in the middle of a basic block
        // Therefore, break
        break;
      }
    }
  }
  else {
    boost::apply_visitor(StmtVisitor{ctx, scope_arg, new_scope, new_stmt_ctx},
                         statement);
  }

  // The presence of a terminator means that there was a return statement
  if (ctx.builder.GetInsertBlock()->getTerminator())
    createDestructBB(ctx, new_stmt_ctx, new_scope, true);
  else {
    ctx.builder.CreateBr(new_stmt_ctx.destruct_bb);
    createDestructBB(ctx, new_stmt_ctx, new_scope, false);
  }
}

} // namespace twinkle::codegen
