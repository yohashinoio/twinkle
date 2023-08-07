/**
 * These codes are licensed under MIT License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _4e85d9d0_bc81_11ec_8422_0242ac120002
#define _4e85d9d0_bc81_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>
#include <twinkle/codegen/codegen.hpp>
#include <twinkle/codegen/exception.hpp>

namespace twinkle::codegen
{

struct StmtContext {
  StmtContext(llvm::BasicBlock* const destruct_bb,
              llvm::AllocaInst* const return_var,
              llvm::BasicBlock* const end_bb,
              llvm::BasicBlock* const break_bb,
              llvm::BasicBlock* const continue_bb) noexcept
    : destruct_bb{destruct_bb}
    , return_var{return_var}
    , end_bb{end_bb}
    , break_bb{break_bb}
    , continue_bb{continue_bb}
  {
  }

  // This is a basic block that destructs objects.
  llvm::BasicBlock* destruct_bb;

  // Store the return value in this variable
  llvm::AllocaInst* return_var;

  // All ret instructions in llvm ir are executed in this basic block
  llvm::BasicBlock* end_bb;

  // Used when a break statement is called
  // If not in a loop, nullptr
  llvm::BasicBlock* break_bb;

  // Used when a continue statement is called
  // If not in a loop, nullptr
  llvm::BasicBlock* continue_bb;
};

// Class that wraps llvm::Value
struct Value {
  Value(llvm::Value* value, const std::shared_ptr<Type>& type)
    : value{value}
    , type{type}
  {
  }

  Value() = delete;

  [[nodiscard]] llvm::Value* getValue() const noexcept
  {
    return value;
  }

  [[nodiscard]] llvm::Type* getLLVMType() const
  {
    return value->getType();
  }

  [[nodiscard]] std::shared_ptr<Type> getType() const
  {
    return type;
  }

  [[nodiscard]] bool isMutable() const noexcept
  {
    return type->isMutable();
  }

  [[nodiscard]] bool isSigned(CGContext& ctx) const
  {
    return type->isSigned(ctx);
  }

private:
  llvm::Value* value;

  std::shared_ptr<Type> type;
};

// Various variable classes inherit this class
struct Variable {
  [[nodiscard]] virtual Value getValue(CGContext& ctx) const = 0;

  [[nodiscard]] virtual llvm::AllocaInst* getAllocaInst() const noexcept = 0;

  [[nodiscard]] virtual std::shared_ptr<Type> getType() const = 0;

  [[nodiscard]] virtual bool isSigned(CGContext& ctx) const = 0;

  [[nodiscard]] virtual bool isMutable() const = 0;
};

struct AllocaVariable : public Variable {
  AllocaVariable(CGContext&   ctx,
                 const Value& alloca,
                 const bool   is_mutable) noexcept
    : alloca{alloca}
    , is_mutable{is_mutable}
  {
    assert(llvm::dyn_cast<llvm::AllocaInst>(alloca.getValue()));

    this->alloca.getType()->setMutable(ctx, is_mutable);
  }

  AllocaVariable() = delete;

  [[nodiscard]] Value getValue(CGContext& ctx) const override
  {
    return {ctx.builder.CreateLoad(getAllocaInst()->getAllocatedType(),
                                   getAllocaInst()),
            getType()};
  }

  [[nodiscard]] llvm::AllocaInst* getAllocaInst() const noexcept override
  {
    return llvm::cast<llvm::AllocaInst>(alloca.getValue());
  }

  [[nodiscard]] std::shared_ptr<Type> getType() const override
  {
    return alloca.getType();
  }

  [[nodiscard]] bool isSigned(CGContext& ctx) const override
  {
    return alloca.isSigned(ctx);
  }

  [[nodiscard]] bool isMutable() const override
  {
    return is_mutable;
  }

private:
  Value alloca;

  const bool is_mutable;
};

// Returns a AST of a class template and a namespace information where it is
// located
[[nodiscard]] std::optional<std::pair<ClassTemplateTableValue, NamespaceStack>>
findClassTemplate(CGContext&                    ctx,
                  const std::string_view        name,
                  const ast::TemplateArguments& args);

// Add template arguments as aliases
// Clean up in destructor
struct TemplateArgumentsDefiner {
  TemplateArgumentsDefiner(CGContext&                     ctx,
                           const ast::TemplateArguments&  args,
                           const ast::TemplateParameters& params,
                           const PositionRange&           pos)
    : ctx{ctx}
    , args{args}
    , params{params}
  {
    insertToAliasTable(pos);
  }

  ~TemplateArgumentsDefiner()
  {
    cleanup();
  }

  void insertToAliasTable(const PositionRange& pos) const;

  void cleanup() const
  {
    ctx.template_argument_tables.pop();
  }

private:
  CGContext& ctx;

  const ast::TemplateArguments&  args;
  const ast::TemplateParameters& params;
};

// Create an alloca instruction in the entry block of
// the function.
[[nodiscard]] llvm::AllocaInst* createEntryAlloca(llvm::Function*    func,
                                                  const std::string& name,
                                                  llvm::Type* const  type);

[[nodiscard]] llvm::Type* getFloatNTy(CGContext& ctx, const int mantissa_width);

/*
  |--------------------------------|
  |     Left |    Right |   Result |
  |----------|----------|----------|
  |   signed |   signed |   signed |
  |   signed | unsigned |   signed |
  | unsigned |   signed |   signed |
  | unsigned | unsigned | unsigned |
  |--------------------------------|
*/
[[nodiscard]] SignKind
logicalOrSign(CGContext& ctx, const Value& lhs, const Value& rhs);

// If either of them is signed, the signed type is returned. Otherwise,
// unsigned.
// Assuming the type is the same.
[[nodiscard]] std::shared_ptr<Type>
resultIntegerTypeOf(CGContext&                   ctx,
                    const std::shared_ptr<Type>& lhs_t,
                    const std::shared_ptr<Type>& rhs_t);

[[nodiscard]] Value createAddInverse(CGContext& ctx, const Value& value);

[[nodiscard]] Value
createAdd(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createSub(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createMul(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createDiv(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createMod(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createNotEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLessThan(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createGreaterThan(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLessOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createGreaterOrEqual(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLogicalAnd(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createLogicalOr(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createShiftLeft(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createShiftRight(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createBitwiseAnd(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createBitwiseOr(CGContext& ctx, const Value& lhs, const Value& rhs);

[[nodiscard]] Value
createDereference(CGContext& ctx, const PositionRange& pos, const Value& val);

[[nodiscard]] Value createDereference(CGContext&                       ctx,
                                      const PositionRange&             pos,
                                      const std::shared_ptr<Variable>& operand);

[[nodiscard]] bool equals(CGContext&                   ctx,
                          const std::shared_ptr<Type>& left,
                          const std::shared_ptr<Type>& right);

} // namespace twinkle::codegen

#endif
