/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <emera/codegen/expr.hpp>
#include <emera/codegen/exception.hpp>
#include <emera/codegen/kind.hpp>
#include <emera/codegen/stmt.hpp>

namespace emera::codegen
{

[[nodiscard]] llvm::Function*
findFunction(CGContext& ctx,
             const std::vector<std::string>&
               mangled_names /* Assuming they are in order of priority */)
{
  for (const auto& r : mangled_names) {
    if (auto const f = ctx.module->getFunction(r))
      return f;
  }

  return nullptr;
}

[[nodiscard]] static std::vector<llvm::Value*>
toLLVMVals(const std::deque<Value>& v)
{
  std::vector<llvm::Value*> ret;

  for (const auto& value : v)
    ret.push_back(value.getValue());

  return ret;
}

//===----------------------------------------------------------------------===//
// Expression visitor
//===----------------------------------------------------------------------===//

struct ExprVisitor : public boost::static_visitor<Value> {
  ExprVisitor(CGContext&         ctx,
              const SymbolTable& scope,
              const StmtContext& stmt_ctx) noexcept
    : ctx{ctx}
    , scope{scope}
    , stmt_ctx{stmt_ctx}
  {
  }

  [[nodiscard]] Value operator()(boost::blank) const
  {
    unreachable();
  }

  // Floating point literals.
  [[nodiscard]] Value operator()(const double node) const
  {
    return {llvm::ConstantFP::get(ctx.builder.getDoubleTy(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::f64)};
  }

  // 32bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint32_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::u32)};
  }

  // 32bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int32_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt32Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::i32)};
  }

  // 64bit unsigned integer literals.
  [[nodiscard]] Value operator()(const std::uint64_t node) const
  {
    return {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::u64)};
  }

  // 64bit signed integer literals.
  [[nodiscard]] Value operator()(const std::int64_t node) const
  {
    return {llvm::ConstantInt::getSigned(ctx.builder.getInt64Ty(), node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::i64)};
  }

  // Boolean literals.
  [[nodiscard]] Value operator()(const bool node) const
  {
    return {llvm::ConstantInt::get(
              BuiltinType{BuiltinTypeKind::bool_}.getLLVMType(ctx),
              node),
            std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  [[nodiscard]] Value operator()(const ast::StringLiteral& node) const
  {
    return {
      ctx.builder.CreateGlobalStringPtr(unicode::utf32toUtf8(node.str), ".str"),
      std::make_shared<PointerType>(
        std::make_shared<BuiltinType>(BuiltinTypeKind::i8))};
  }

  [[nodiscard]] Value operator()(const ast::CharLiteral& node) const
  {
    // Unicode code point.
    return {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), node.ch),
            std::make_shared<BuiltinType>(BuiltinTypeKind::char_)};
  }

  [[nodiscard]] Value operator()(const ast::ArrayLiteral& node) const
  {
    std::vector<Value> initializer_list;
    initializer_list.reserve(node.elements.size());

    for (const auto& elem : node.elements)
      initializer_list.push_back(boost::apply_visitor(*this, elem));

    const auto type
      = std::make_shared<ArrayType>(initializer_list.front().getType(),
                                    initializer_list.size());

    auto const alloca
      = createEntryAlloca(ctx.builder.GetInsertBlock()->getParent(),
                          "",
                          type->getLLVMType(ctx));

    for (std::size_t idx = 0; const auto& initializer : initializer_list) {
      auto const gep = ctx.builder.CreateInBoundsGEP(
        alloca->getAllocatedType(),
        alloca,
        {llvm::ConstantInt::get(ctx.builder.getInt64Ty(), 0),
         llvm::ConstantInt::get(ctx.builder.getInt64Ty(), idx)});

      ctx.builder.CreateStore(initializer_list.at(idx++).getValue(), gep);
    }

    return {ctx.builder.CreateLoad(alloca->getAllocatedType(), alloca), type};
  }

  [[nodiscard]] Value operator()(const ast::BuiltinMacro& node) const
  {
    switch (node.kind) {
    case BuiltinMacroKind::huge_valf:
      [[fallthrough]];

    case BuiltinMacroKind::infinity_:
      return {llvm::ConstantFP::getInfinity(ctx.builder.getFloatTy()),
              std::make_shared<BuiltinType>(BuiltinTypeKind::f32)};

    case BuiltinMacroKind::huge_val:
      return {llvm::ConstantFP::getInfinity(ctx.builder.getDoubleTy()),
              std::make_shared<BuiltinType>(BuiltinTypeKind::f64)};

    case BuiltinMacroKind::unknown:
      unreachable();
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::Identifier& node) const
  {
    const auto variable_refwrap = findVariable(node);

    if (!variable_refwrap) {
      const auto member = findMemberOfThis(node);

      if (member)
        return *member;
      else {
        throw CodegenError{ctx.formatError(
          ctx.positions.position_of(node),
          fmt::format("unknown variable '{}' referenced", node.utf8()))};
      }
    }

    const auto variable = variable_refwrap->get();

    auto const loaded_var = loadVariable(variable);

    if (variable.getType()->isRefTy(ctx)) {
      // Since reference types wrap pointer types
      return createDereference(ctx,
                               ctx.positions.position_of(node),
                               loaded_var);
    }

    return loaded_var;
  }

  [[nodiscard]] Value operator()(const ast::MemberAccess& node) const
  {
    if (const auto* rhs = boost::get<ast::Identifier>(&node.rhs))
      return memberVariableAccess(node.lhs, *rhs);

    if (const auto* rhs = boost::get<ast::FunctionCall>(&node.rhs)) {
      try {
        return methodAccess(node.lhs, *rhs);
      }
      catch (const CodegenError&) {
        if (ctx.ns_hierarchy.empty())
          throw;

        if (ctx.ns_hierarchy.top().kind == NamespaceKind::class_) {
          // Assuming access to methods of member variables
          const auto top = ctx.ns_hierarchy.pop();
          const auto tmp = methodAccess(node.lhs, *rhs);
          ctx.ns_hierarchy.push(top);
          return tmp;
        }
      }
    }

    throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                       "cannot generate right-hand side")};
  }

  [[nodiscard]] Value operator()(const ast::Subscript& node) const
  {
    const auto value = createNoLoadSubscript(node);

    return {ctx.builder.CreateLoad(value.getLLVMType()->getPointerElementType(),
                                   value.getValue()),
            value.getType(),
            value.isMutable()};
  }

  [[nodiscard]] Value operator()(const ast::BinOp& node) const
  {
    const auto uncasted_lhs = boost::apply_visitor(*this, node.lhs);
    const auto uncasted_rhs = boost::apply_visitor(*this, node.rhs);

    const auto [lhs, rhs] = castToLarger(uncasted_lhs, uncasted_rhs);

    switch (node.kind()) {
    case ast::BinOp::Kind::add:
      return createAdd(ctx, lhs, rhs);

    case ast::BinOp::Kind::sub:
      return createSub(ctx, lhs, rhs);

    case ast::BinOp::Kind::mul:
      return createMul(ctx, lhs, rhs);

    case ast::BinOp::Kind::div:
      return createDiv(ctx, lhs, rhs);

    case ast::BinOp::Kind::mod:
      return createMod(ctx, lhs, rhs);

    case ast::BinOp::Kind::eq:
      return createEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::neq:
      return createNotEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::lt:
      return createLessThan(ctx, lhs, rhs);

    case ast::BinOp::Kind::gt:
      return createGreaterThan(ctx, lhs, rhs);

    case ast::BinOp::Kind::le:
      return createLessOrEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::ge:
      return createGreaterOrEqual(ctx, lhs, rhs);

    case ast::BinOp::Kind::logical_and:
      return createLogicalAnd(ctx, lhs, rhs);

    case ast::BinOp::Kind::logical_or:
      return createLogicalOr(ctx, lhs, rhs);

    case ast::BinOp::Kind::bitwise_shift_left:
      return createShiftLeft(ctx, lhs, rhs);

    case ast::BinOp::Kind::bitwise_shift_right:
      return createShiftRight(ctx, lhs, rhs);

    case ast::BinOp::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        fmt::format("unknown operator '{}' detected", node.opstr()))};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::UnaryOp& node) const
  {
    auto const operand_val = boost::apply_visitor(*this, node.operand);

    if (!operand_val.getValue()) {
      throw CodegenError{ctx.formatError(ctx.positions.position_of(node),
                                         "failed to generate right-hand side")};
    }

    switch (node.kind()) {
    case ast::UnaryOp::Kind::plus:
      return operand_val;

    case ast::UnaryOp::Kind::minus:
      return createAddInverse(ctx, operand_val);

    case ast::UnaryOp::Kind::not_:
      return createLogicalNot(operand_val);

    case ast::UnaryOp::Kind::address_of:
      return createAddressOf(operand_val);

    case ast::UnaryOp::Kind::size_of:
      return createSizeOf(operand_val);

    case ast::UnaryOp::Kind::unknown:
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        fmt::format("unknown operator '{}' detected", node.opstr()))};
    }

    unreachable();
  }

  [[nodiscard]] Value operator()(const ast::Reference& node) const
  {
    return createReference(boost::apply_visitor(*this, node.operand));
  }

  [[nodiscard]] Value operator()(const ast::New& node) const
  {
    const auto type = createType(node.type);

    const auto is_class_ty = type->isClassTy(ctx);

    if (!is_class_ty && node.with_init) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "cannot initialize non-class with new operator")};
    }

    auto const llvm_type = type->getLLVMType(ctx);

    auto const malloc_inst = llvm::CallInst::CreateMalloc(
      ctx.builder.GetInsertBlock(),
      ctx.builder.getIntPtrTy(ctx.module->getDataLayout()),
      llvm_type,
      createSizeOf(llvm_type).getValue(),
      nullptr,
      nullptr);

    // If not inserted (builder.Insert), malloc will be badref
    ctx.builder.Insert(malloc_inst);

    const auto malloc_return_type = std::make_shared<PointerType>(type);

    if (is_class_ty && node.with_init) {
      auto args
        = createArgVals(node.initializer, ctx.positions.position_of(node));

      // Push 'this' pointer
      args.push_front({malloc_inst, malloc_return_type});

      createConstructorCall(ctx.positions.position_of(node),
                            type->getClassName(ctx),
                            args);
    }

    return {malloc_inst, malloc_return_type};
  }

  [[nodiscard]] Value operator()(const ast::Delete& node) const
  {
    const auto operand_val = boost::apply_visitor(*this, node.operand);

    if (!operand_val.getValue()->getType()->isPointerTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "cannot delete expression of the type")};
    }

    auto const derefed_operand_val
      = createDereference(ctx, ctx.positions.position_of(node), operand_val);

    if (derefed_operand_val.getType()->isClassTy(ctx))
      invokeDestructor(ctx, derefed_operand_val);

    // If not inserted (builder.Insert), free will be badref
    ctx.builder.Insert(
      llvm::CallInst::CreateFree(operand_val.getValue(),
                                 ctx.builder.GetInsertBlock()));

    return {nullptr, std::make_shared<BuiltinType>(BuiltinTypeKind::void_)};
  }

  [[nodiscard]] Value operator()(const ast::Dereference& node) const
  {
    const auto value = boost::apply_visitor(*this, node.operand);

    return createDereference(ctx, ctx.positions.position_of(node), value);
  }

  [[nodiscard]] Value operator()(const ast::FunctionCall& node) const
  {
    const auto pos = ctx.positions.position_of(node);

    if (node.callee.type() != typeid(ast::Identifier)) {
      throw CodegenError{
        ctx.formatError(pos,
                        "left-hand side of function call is not callable")};
    }

    const auto callee_name = boost::get<ast::Identifier>(node.callee).utf8();

    auto args = createArgVals(node.args, pos);

    if (auto const func = findCalleeMethod(callee_name, args)) {
      args.push_front((*this)(ast::Identifier{std::u32string{U"this"}}));
      return createFunctionCall(func, args, pos);
    }

    if (auto const func = findCalleeFunc(callee_name, args))
      return createFunctionCall(func, args, pos);

    throw CodegenError{ctx.formatError(
      pos,
      fmt::format("unknown function '{}' referenced", callee_name))};
  }

  [[nodiscard]] Value operator()(const ast::Cast& node) const
  {
    auto const lhs = boost::apply_visitor(*this, node.lhs);

    const auto as = createType(node.as);

    if (as->isPointerTy(ctx)) {
      // Pointer to pointer.
      return {
        ctx.builder.CreatePointerCast(lhs.getValue(), as->getLLVMType(ctx)),
        as};
    }

    if (as->isFloatingPointTy(ctx)) {
      if (lhs.getType()->isIntegerTy(ctx)) {
        const auto cast_op = lhs.getType()->isSigned(ctx)
                               ? llvm::CastInst::CastOps::SIToFP
                               : llvm::CastInst::CastOps::UIToFP;

        return {
          ctx.builder.CreateCast(cast_op, lhs.getValue(), as->getLLVMType(ctx)),
          as};
      }

      // Floating point number to floating point number.
      return {ctx.builder.CreateFPCast(lhs.getValue(), as->getLLVMType(ctx)),
              as};
    }

    if (as->getLLVMType(ctx)->isIntegerTy()) {
      if (lhs.getType()->isFloatingPointTy(ctx)) {
        // Floating point number to integer.
        const auto cast_op = as->isSigned(ctx)
                               ? llvm::CastInst::CastOps::FPToSI
                               : llvm::CastInst::CastOps::FPToUI;

        return {
          ctx.builder.CreateCast(cast_op, lhs.getValue(), as->getLLVMType(ctx)),
          as};
      }

      // Integer to integer.
      return {ctx.builder.CreateIntCast(lhs.getValue(),
                                        as->getLLVMType(ctx),
                                        as->isSigned(ctx)),
              as};
    }

    throw CodegenError{
      ctx.formatError(ctx.positions.position_of(node), "non-convertible type")};
  }

  [[nodiscard]] Value operator()(const ast::Pipeline& node) const
  {
    if (node.rhs.type() != typeid(ast::FunctionCall)) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(node),
        "the right side of the pipeline requires a function call")};
    }

    auto call = boost::get<ast::FunctionCall>(node.rhs);

    call.args.push_front(node.lhs);

    return (*this)(call);
  }

  [[nodiscard]] Value operator()(const ast::ClassLiteral& node) const
  {
    const auto class_type = findClass(node.class_name.utf8());

    const auto pos = ctx.positions.position_of(node);

    if (!class_type) {
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("class {} is undefined", node.class_name.utf8()))};
    }

    const auto class_name = class_type.value()->getClassName(ctx);

    auto const alloca
      = createEntryAlloca(ctx.builder.GetInsertBlock()->getParent(),
                          "",
                          class_type.value()->getLLVMType(ctx));

    const auto this_pointer_type = std::make_shared<PointerType>(
      std::make_shared<UserDefinedType>(class_name));

    auto args = createArgVals(node.initializer_list, pos);

    // Push 'this' pointer
    args.push_front({alloca, this_pointer_type});

    createConstructorCall(pos, class_name, args);

    return {ctx.builder.CreateLoad(alloca->getAllocatedType(), alloca),
            this_pointer_type->getPointeeType(ctx)};
  }

private:
  void
  createConstructorCall(const boost::iterator_range<emera::InputIterator>& pos,
                        const std::string&       class_name,
                        const std::deque<Value>& args) const
  {
    ctx.ns_hierarchy.push({class_name, NamespaceKind::class_});

    const auto mangleds = ctx.mangler.mangleConstructorCall(ctx, args);

    ctx.ns_hierarchy.pop();

    if (auto func = findFunction(ctx, mangleds)) {
      // Ignore the return value since constructors have no return value
      static_cast<void>(createFunctionCall(func, args, pos));
    }
    else {
      throw CodegenError{ctx.formatError(
        pos,
        fmt::format("no matching constructor for initialization of {}",
                    class_name))};
    }
  }

  [[nodiscard]] Value loadVariable(const Variable& variable) const
  {
    return {ctx.builder.CreateLoad(variable.getAllocaInst()->getAllocatedType(),
                                   variable.getAllocaInst()),
            variable.getType(),
            variable.isMutable()};
  }

  [[nodiscard]] std::optional<std::shared_ptr<Type>>
  findClass(const std::string& name) const
  {
    if (const auto alias = ctx.alias_table[name];
        alias && alias.value()->isClassTy(ctx)) {
      return *alias;
    }

    if (const auto class_ = ctx.class_table[name];
        class_ && class_.value()->isClassTy(ctx)) {
      return *class_;
    }

    return std::nullopt;
  }

  [[nodiscard]] Value createFunctionCall(
    llvm::Function* const                              callee_func,
    const std::deque<Value>&                           args,
    const boost::iterator_range<emera::InputIterator>& pos) const
  {
    if (!callee_func->isVarArg() && callee_func->arg_size() != args.size())
      throw CodegenError{ctx.formatError(pos, "incorrect arguments passed")};

    verifyArguments(args, callee_func, pos);

    const auto return_type = ctx.return_type_table[callee_func];

    assert(return_type);

    return {ctx.builder.CreateCall(callee_func, toLLVMVals(args)),
            *return_type};
  }

  // Note that the return value is a reference,
  // so be careful about the lifetime.
  [[nodiscard]] std::optional<std::reference_wrapper<const Variable>>
  findVariable(const ast::Identifier& node) const
  {
    const auto ident = node.utf8();

    if (const auto variable = scope[ident])
      return variable;

    return std::nullopt;
  }

  // Find a member of '*this'
  [[nodiscard]] std::optional<Value>
  findMemberOfThis(const ast::Identifier& node) const
  {
    if (scope["this"]) {
      const auto this_p
        = findVariable(ast::Identifier{std::u32string{U"this"}});

      if (!this_p)
        return std::nullopt;

      const auto this_v
        = createDereference(ctx, ctx.positions.position_of(node), *this_p);

      return memberVariableAccess(this_v, node, false);
    }

    return std::nullopt;
  }

  // For Integer
  [[nodiscard]] std::pair<Value, Value> toLargerBitWidth(const Value& lhs,
                                                         const Value& rhs) const
  {
    if (const auto lhs_bitwidth = lhs.getLLVMType()->getIntegerBitWidth(),
        rhs_bitwidth            = rhs.getLLVMType()->getIntegerBitWidth();
        lhs_bitwidth != rhs_bitwidth) {
      const auto larger_bitwidth = std::max(lhs_bitwidth, rhs_bitwidth);

      const auto is_target_lhs = lhs_bitwidth == larger_bitwidth;

      const auto target_type = is_target_lhs ? lhs.getType() : rhs.getType();

      if (is_target_lhs) {
        return std::make_pair(
          lhs,
          Value{ctx.builder.CreateIntCast(rhs.getValue(),
                                          lhs.getLLVMType(),
                                          target_type->isSigned(ctx)),
                target_type});
      }
      else {
        return std::make_pair(
          Value{ctx.builder.CreateIntCast(lhs.getValue(),
                                          rhs.getLLVMType(),
                                          target_type->isSigned(ctx)),
                target_type},
          rhs);
      }
    }

    return std::make_pair(lhs, rhs);
  }

  // For floating point number
  [[nodiscard]] std::pair<Value, Value>
  toLargerMantissaWidth(const Value& lhs, const Value& rhs) const
  {
    if (const auto lhs_mantissaw = lhs.getLLVMType()->getFPMantissaWidth(),
        rhs_mantissaw            = rhs.getLLVMType()->getFPMantissaWidth();
        lhs_mantissaw != rhs_mantissaw) {
      const auto larger_mantissaw = std::max(lhs_mantissaw, rhs_mantissaw);

      const auto is_target_lhs = lhs_mantissaw == larger_mantissaw;

      const auto target_type = is_target_lhs ? lhs.getType() : rhs.getType();

      if (is_target_lhs) {
        return std::make_pair(
          lhs,
          Value{ctx.builder.CreateFPCast(rhs.getValue(), lhs.getLLVMType()),
                target_type});
      }
      else {
        return std::make_pair(
          Value{ctx.builder.CreateFPCast(lhs.getValue(), rhs.getLLVMType()),
                target_type},
          rhs);
      }
    }

    return std::make_pair(lhs, rhs);
  }

  // Cast to larger bit(mantissa) width.
  [[nodiscard]] std::pair<Value, Value> castToLarger(const Value& lhs,
                                                     const Value& rhs) const
  {
    if (lhs.getValue()->getType()->isIntegerTy())
      return toLargerBitWidth(lhs, rhs);

    if (lhs.getValue()->getType()->isFloatingPointTy())
      return toLargerMantissaWidth(lhs, rhs);

    return std::make_pair(lhs, rhs);
  }

  [[nodiscard]] Value createPointerToArray(const Value& array) const
  {
    return {llvm::getPointerOperand(array.getValue()),
            std::make_shared<PointerType>(array.getType()),
            array.isMutable()};
  }

  [[nodiscard]] Value createArraySubscript(const Value& array,
                                           const Value& index) const
  {
    const auto p_to_array = createPointerToArray(array);

    // Calculate the address of the index-th element.
    auto const gep = ctx.builder.CreateInBoundsGEP(
      p_to_array.getLLVMType()->getPointerElementType(),
      p_to_array.getValue(),
      {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0), index.getValue()});

    return {gep,
            p_to_array.getType()->getPointeeType(ctx)->getArrayElementType(ctx),
            p_to_array.isMutable()};
  }

  [[nodiscard]] Value createPointerSubscript(const Value& ptr,
                                             const Value& index) const
  {
    // Calculate the address of the index-th element.
    auto const gep = ctx.builder.CreateInBoundsGEP(
      ptr.getLLVMType()->getPointerElementType(),
      ptr.getValue(),
      index.getValue());

    return {gep, ptr.getType()->getPointeeType(ctx), ptr.isMutable()};
  }

  // Normally a subscript operation calls createLoad at the end, but this
  // function does not.
  [[nodiscard]] Value createNoLoadSubscript(const ast::Subscript& node) const
  {
    auto lhs = createExpr(ctx, scope, stmt_ctx, node.lhs);

    const auto is_array = lhs.getType()->isArrayTy(ctx);

    if (!is_array && !lhs.getType()->isPointerTy(ctx)) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "the type incompatible with the subscript operator")};
    }

    const auto index = createExpr(ctx, scope, stmt_ctx, node.subscript);

    if (!index.getValue()->getType()->isIntegerTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(node),
                        "subscripts need to be evaluated to numbers")};
    }

    return is_array ? createArraySubscript(lhs, index)
                    : createPointerSubscript(lhs, index);
  }

  [[nodiscard]] Value createLogicalNot(const Value& value) const
  {
    if (value.getType()->isFloatingPointTy(ctx)) {
      return {
        ctx.builder.CreateFCmp(llvm::ICmpInst::FCMP_OEQ,
                               value.getValue(),
                               llvm::ConstantFP::get(value.getLLVMType(), 0)),
        std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
    }

    return {
      ctx.builder.CreateICmp(llvm::ICmpInst::ICMP_EQ,
                             value.getValue(),
                             llvm::ConstantInt::get(value.getLLVMType(), 0)),
      std::make_shared<BuiltinType>(BuiltinTypeKind::bool_)};
  }

  [[nodiscard]] Value createSizeOf(llvm::Type* const type) const
  {
    // Assuming a 64-bit environment.
    // FIXME: To work in different environments
    return {llvm::ConstantInt::get(
              ctx.builder.getInt64Ty(),
              ctx.module->getDataLayout().getTypeAllocSize(type)),
            std::make_shared<BuiltinType>(BuiltinTypeKind::u64)};
  }

  [[nodiscard]] Value createSizeOf(const Value& value) const
  {
    return createSizeOf(value.getLLVMType());
  }

  [[nodiscard]] Value createReference(const Value& val) const
  {
    return {createAddressOf(val).getValue(),
            std::make_shared<ReferenceType>(val.getType())};
  }

  // Do not use for constants!
  [[nodiscard]] Value createAddressOf(const Value& val) const
  {
    auto ptr = llvm::getPointerOperand(val.getValue());
    assert(ptr);
    return {ptr, std::make_shared<PointerType>(val.getType())};
  }

  void verifyArguments(const std::deque<Value>&                    args,
                       llvm::Function* const                       callee,
                       const boost::iterator_range<InputIterator>& pos) const
  {
    for (std::size_t idx = 0; auto&& arg : callee->args()) {
      if (!strictEquals(args[idx++].getValue()->getType(), arg.getType())) {
        throw CodegenError{ctx.formatError(
          pos,
          fmt::format("incompatible type for argument {}", idx))};
      }
    }
  }

  [[nodiscard]] std::deque<Value>
  createArgVals(const std::vector<ast::Expr>&               exprs,
                const boost::iterator_range<InputIterator>& pos) const
  {
    std::deque<Value> args;

    for (const auto& r : exprs)
      args.push_back(boost::apply_visitor(*this, r));

    return args;
  }

  [[nodiscard]] std::deque<Value>
  createArgVals(const std::deque<ast::Expr>&                exprs,
                const boost::iterator_range<InputIterator>& pos) const
  {
    std::deque<Value> args;

    for (const auto& r : exprs)
      args.push_back(boost::apply_visitor(*this, r));

    return args;
  }

  [[nodiscard]] llvm::Function*
  findVarArgFunction(const std::vector<std::string>& mangled_names) const
  {
    const auto f = [&](const std::string_view mangled) -> llvm::Function* {
      for (auto& func : ctx.module->getFunctionList()) {
        const auto func_name = func.getName();

        if (func_name.endswith(mangle::ellipsis)) {
          // _Z1fv to _Z1f
          const auto tmp = func_name.substr(0, func_name.size() - 2);
          if (mangled.starts_with(tmp))
            return &func;
        }
      }

      return nullptr;
    };

    for (const auto& r : mangled_names) {
      if (auto const tmp = f(r))
        return tmp;
    }

    return nullptr;
  }

  // The innermost namespace is inserted at the beginning of the argument as
  // 'this'
  // Used to search for methods in the same class as itself
  [[nodiscard]] llvm::Function*
  findCalleeMethod(const std::string&       unmangled_name,
                   const std::deque<Value>& args) const
  {
    if (ctx.ns_hierarchy.empty()
        || ctx.ns_hierarchy.top().kind != NamespaceKind::class_) {
      return nullptr;
    }

    const auto f = [&](const Accessibility accessibility) {
      const auto mangled_names
        = ctx.mangler.mangleMethodCall(ctx,
                                       unmangled_name,
                                       ctx.ns_hierarchy.top().name,
                                       args,
                                       accessibility);

      const auto func = findFunction(ctx, mangled_names);

      if (func)
        return func;
      else
        return findVarArgFunction(mangled_names);

      unreachable();
    };

    if (auto const func = f(Accessibility::public_))
      return func;
    else if (auto const func = f(Accessibility::private_))
      return func;
    else
      return nullptr;

    unreachable();
  }

  [[nodiscard]] llvm::Function*
  findCalleeFunc(const std::string_view   unmangled_name,
                 const std::deque<Value>& args) const
  {
    {
      // First look for unmangled functions
      auto const func = ctx.module->getFunction(unmangled_name);
      if (func)
        return func;
    }

    const auto mangled_names
      = ctx.mangler.mangleFunctionCall(ctx, unmangled_name, args);

    auto const func = findFunction(ctx, mangled_names);

    if (!func) {
      // Mismatch or variadic arguments.
      auto const vararg_func = findVarArgFunction(mangled_names);

      if (vararg_func)
        return vararg_func;
      else
        return nullptr;
    }

    return func;
  }

  [[nodiscard]] Value
  memberVariableAccess(const Value&           class_val,
                       const ast::Identifier& member_name_ast,
                       const bool             external_access = true) const
  {
    const auto member_name = member_name_ast.utf8();

    if (!class_val.getLLVMType()->isStructTy()) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(member_name_ast),
                        "member access cannot be used for non-class")};
    }

    const auto class_type
      = ctx.class_table[class_val.getLLVMType()->getStructName().str()];

    if (!class_type || class_type.value()->isOpaque(ctx)) {
      throw CodegenError{
        ctx.formatError(ctx.positions.position_of(member_name_ast),
                        "member access to undefined class is not allowed")};
    }

    const auto offset = class_type.value()->offsetByName(member_name);

    if (!offset) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(member_name_ast),
        fmt::format("undefined member '{}' selected", member_name))};
    }

    const auto& member_info = class_type.value()->getMemberVar(*offset);

    if (external_access && !isExternallyAccessible(member_info.accessibility)) {
      throw CodegenError{ctx.formatError(
        ctx.positions.position_of(member_name_ast),
        fmt::format("member '{}' is not accessible", member_name))};
    }

    auto const lhs_address = llvm::getPointerOperand(class_val.getValue());

    auto const gep = ctx.builder.CreateInBoundsGEP(
      class_val.getLLVMType(),
      lhs_address,
      {llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0),
       llvm::ConstantInt::get(ctx.builder.getInt32Ty(), *offset)});

    return {ctx.builder.CreateLoad(
              class_val.getLLVMType()->getStructElementType(*offset),
              gep),
            member_info.type,
            member_info.is_mutable};
  }

  [[nodiscard]] Value
  memberVariableAccess(const ast::Expr&       class_,
                       const ast::Identifier& member_name_ast,
                       const bool             external_access = true) const
  {
    return memberVariableAccess(createExpr(ctx, scope, stmt_ctx, class_),
                                member_name_ast,
                                external_access);
  }

  [[nodiscard]] Value methodAccess(const ast::Expr&         lhs,
                                   const ast::FunctionCall& rhs) const
  {
    auto func_call = rhs; // Copy

    // Insert 'this' pointer at the beginning of the arguments
    func_call.args.push_front(ast::UnaryOp{std::u32string{U"&"}, lhs});

    {
      // FIXME: More efficient
      const auto lhs_value = createExpr(ctx, scope, stmt_ctx, lhs);

      assert(lhs_value.getType()->isClassTy(ctx));

      ctx.ns_hierarchy.push(
        {lhs_value.getType()->getClassName(ctx), NamespaceKind::class_});
    }

    try {
      const auto retval = (*this)(func_call);
      ctx.ns_hierarchy.pop();
      return retval;
    }
    catch (const CodegenError&) {
      ctx.ns_hierarchy.pop();
      throw;
    }

    unreachable();
  }

  CGContext& ctx;

  const SymbolTable& scope;

  const StmtContext& stmt_ctx;
};

[[nodiscard]] Value createExpr(CGContext&         ctx,
                               const SymbolTable& scope,
                               const StmtContext& stmt_ctx,
                               const ast::Expr&   expr)
{
  return boost::apply_visitor(ExprVisitor{ctx, scope, stmt_ctx}, expr);
}

} // namespace emera::codegen
