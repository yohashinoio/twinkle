//
//  codegen.cpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#include "codegen.hpp"
#include "utility.hpp"

namespace miko::codegen
{

// It seems that the member function has to be const.
struct operand_visitor : public boost::static_visitor<llvm::Value*> {
  operand_visitor(llvm::IRBuilder<>&           builder,
                  const std::filesystem::path& filepath)
    : builder{builder}
    , filepath{filepath}
  {
  }

  llvm::Value* operator()(const ast::nil&) const
  {
    // Program was blank.
    throw std::runtime_error{
      format_error_message(filepath.string(),
                           "empty programs cannot be compiled.",
                           true)};
    BOOST_ASSERT(0);
    return nullptr;
  }

  llvm::Value* operator()(const int value) const
  {
    return llvm::ConstantInt::get(builder.getInt32Ty(), value);
  }

  llvm::Value* operator()(const ast::unaryop& op) const
  {
    auto rhs = boost::apply_visitor(*this, op.rhs);

    if (op.op == "+")
      return rhs;
    if (op.op == "-") {
      // -x to 0-x
      return apply_sub_op(llvm::ConstantInt::get(builder.getInt32Ty(), 0), rhs);
    }

    BOOST_ASSERT(0);
    return nullptr;
  }

  llvm::Value* operator()(const ast::binop& op) const
  {
    auto lhs = boost::apply_visitor(*this, op.lhs);
    auto rhs = boost::apply_visitor(*this, op.rhs);

    // add
    if (op.op == "+")
      return apply_add_op(lhs, rhs);
    if (op.op == "-")
      return apply_sub_op(lhs, rhs);

    // mul
    if (op.op == "*")
      return apply_mul_op(lhs, rhs);
    if (op.op == "/")
      return apply_div_op(lhs, rhs);

    // equality
    if (op.op == "==")
      return apply_equal(lhs, rhs);
    if (op.op == "!=")
      return apply_not_equal(lhs, rhs);

    // relational
    if (op.op == "<")
      return apply_signed_lt(lhs, rhs);
    if (op.op == ">")
      return apply_signed_gt(lhs, rhs);
    if (op.op == "<=")
      return apply_signed_lte(lhs, rhs);
    if (op.op == ">=")
      return apply_signed_gte(lhs, rhs);

    BOOST_ASSERT(0);
    return nullptr;
  }

private:
  // Since builder is a reference, it can be modified in const member functions.
  llvm::IRBuilder<>& builder;

  const std::filesystem::path& filepath;

  llvm::Value* apply_add_op(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateAdd(lhs, rhs);
  }
  llvm::Value* apply_sub_op(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateSub(lhs, rhs);
  }

  llvm::Value* apply_mul_op(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateMul(lhs, rhs);
  }
  llvm::Value* apply_div_op(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateSDiv(lhs, rhs);
  }

  llvm::Value* apply_equal(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhs, rhs);
  }
  llvm::Value* apply_not_equal(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhs, rhs);
  }

  // Less than
  llvm::Value* apply_signed_lt(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SLT, lhs, rhs);
  }
  // Greater than
  llvm::Value* apply_signed_gt(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SGT, lhs, rhs);
  }
  // Less than or equal to
  llvm::Value* apply_signed_lte(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SLE, lhs, rhs);
  }
  // Greater than or equal to
  llvm::Value* apply_signed_gte(llvm::Value* lhs, llvm::Value* rhs) const
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SGE, lhs, rhs);
  }
};

code_generator::code_generator(const std::filesystem::path& filepath,
                               ast::program&&               ast)
  : builder{context}
  , module{std::make_unique<llvm::Module>(filepath.filename().string(),
                                          context)}
  , filepath{filepath}
  , ast{std::move(ast)}
{
}

auto code_generator::print() const -> void
{
  module->print(llvm::outs(), nullptr);
}

auto code_generator::write_bitcode_to_file(
  const std::filesystem::path& out) const -> void
{
  std::error_code      ec;
  llvm::raw_fd_ostream os{out.string(), ec};

  llvm::WriteBitcodeToFile(*module, os);
  os.close();
}

auto code_generator::codegen() -> void
{
  auto return_type = llvm::FunctionType::get(builder.getInt32Ty(), false);
  auto function    = llvm::Function::Create(return_type,
                                         llvm::Function::ExternalLinkage,
                                         "main",
                                         *module);

  auto bb = llvm::BasicBlock::Create(context, "entry", function);
  builder.SetInsertPoint(bb);

  auto ret_val = boost::apply_visitor(operand_visitor{builder, filepath}, ast);

  builder.CreateRet(ret_val);
}

} // namespace miko::codegen
