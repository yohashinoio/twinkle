//
//  codegen.cpp
//
//  Copyright (c) 2022 The Miko Authors.
//  MIT License
//

#include "codegen.hpp"
#include "utility.hpp"

namespace miko::codegen
{

// It seems that the member function has to be const.
struct expression_visitor : public boost::static_visitor<llvm::Value*> {
  expression_visitor(llvm::IRBuilder<>&            builder,
                     std::shared_ptr<llvm::Module> module,
                     const std::filesystem::path&  source)
    : builder{builder}
    , module{module}
    , source{source}
  {
  }

  auto operator()(ast::nil) const -> llvm::Value*
  {
    BOOST_ASSERT(0);
  }

  auto operator()(const int value) const -> llvm::Value*
  {
    return llvm::ConstantInt::get(builder.getInt32Ty(), value);
  }

  auto operator()(const ast::unaryop& node) const -> llvm::Value*
  {
    auto rhs = boost::apply_visitor(*this, node.rhs);

    if (node.op == "+")
      return rhs;
    if (node.op == "-") {
      // -x to 0-x
      return apply_sub_op(llvm::ConstantInt::get(builder.getInt32Ty(), 0), rhs);
    }

    BOOST_ASSERT_MSG(
      0,
      "Unsupported unary operators may have been converted to ASTs.");
  }

  auto operator()(const ast::binop& node) const -> llvm::Value*
  {
    auto lhs = boost::apply_visitor(*this, node.lhs);
    auto rhs = boost::apply_visitor(*this, node.rhs);

    // addition
    if (node.op == "+")
      return apply_add_op(lhs, rhs);
    if (node.op == "-")
      return apply_sub_op(lhs, rhs);

    // multiplication
    if (node.op == "*")
      return apply_mul_op(lhs, rhs);
    if (node.op == "/")
      return apply_div_op(lhs, rhs);

    // equality
    if (node.op == "==")
      return apply_equal(lhs, rhs);
    if (node.op == "!=")
      return apply_not_equal(lhs, rhs);

    // relational
    if (node.op == "<")
      return apply_signed_lt(lhs, rhs);
    if (node.op == ">")
      return apply_signed_gt(lhs, rhs);
    if (node.op == "<=")
      return apply_signed_lte(lhs, rhs);
    if (node.op == ">=")
      return apply_signed_gte(lhs, rhs);

    BOOST_ASSERT_MSG(
      0,
      "Unsupported binary operators may have been converted to ASTs.");
  }

  auto operator()(ast::variable) const -> llvm::Value*
  {
    // TODO:
    BOOST_ASSERT(0);
  }

  auto operator()(const ast::function_call& node) const -> llvm::Value*
  {
    auto* callee_f = module->getFunction(node.callee);
    if (!callee_f) {
      throw std::runtime_error{format_error_message(
        source.string(),
        format("Unknown function %s referenced", node.callee))};
    }

    // if (callee_f->arg_size() == args.size()) {
    //   throw std::runtime_error{
    //     format_error_message(source.string(),
    //                          format("Incorrect arguments passed"))};
    // }

    // std::vector<llvm::Value*> args_v;
    // for (std::size_t i = 0, size = args.sizse(); i != size; ++i) {
    //   args_v.push_back(this->operator()(args[i]));
    //   if (!args_v.back()) {
    //     throw std::runtime_error{
    //       format_error_message(source.string(), format("Invalid
    //       expression"))};
    //   }
    // }

    return builder.CreateCall(callee_f);
  }

private:
  llvm::IRBuilder<>&            builder;
  std::shared_ptr<llvm::Module> module;

  const std::filesystem::path& source;

  auto apply_add_op(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateAdd(lhs, rhs);
  }
  auto apply_sub_op(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateSub(lhs, rhs);
  }
  auto apply_mul_op(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateMul(lhs, rhs);
  }
  auto apply_div_op(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateSDiv(lhs, rhs);
  }

  auto apply_equal(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhs, rhs);
  }
  auto apply_not_equal(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhs, rhs);
  }

  // Less than
  auto apply_signed_lt(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SLT, lhs, rhs);
  }
  // Greater than
  auto apply_signed_gt(llvm::Value* lhs, llvm::Value* rhs) const -> llvm::Value*
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SGT, lhs, rhs);
  }
  // Less than or equal to
  auto apply_signed_lte(llvm::Value* lhs, llvm::Value* rhs) const
    -> llvm::Value*
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SLE, lhs, rhs);
  }
  // Greater than or equal to
  auto apply_signed_gte(llvm::Value* lhs, llvm::Value* rhs) const
    -> llvm::Value*
  {
    return builder.CreateICmp(llvm::ICmpInst::ICMP_SGE, lhs, rhs);
  }
};

struct statement_visitor : public boost::static_visitor<void> {
  statement_visitor(llvm::IRBuilder<>&            builder,
                    std::shared_ptr<llvm::Module> module,
                    const std::filesystem::path&  source)
    : builder{builder}
    , module{module}
    , source{source}
  {
  }

  auto operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  auto operator()(const ast::expression& node) const
  {
    boost::apply_visitor(expression_visitor{builder, module, source}, node);
  }

  auto operator()(const ast::return_statement& node) const
  {
    auto* retval
      = boost::apply_visitor(expression_visitor{builder, module, source},
                             node.rhs);

    builder.CreateRet(retval);
  }

private:
  llvm::IRBuilder<>&            builder;
  std::shared_ptr<llvm::Module> module;

  const std::filesystem::path& source;
};

struct toplevel_visitor : public boost::static_visitor<llvm::Function*> {
  toplevel_visitor(llvm::LLVMContext&            context,
                   llvm::IRBuilder<>&            builder,
                   std::shared_ptr<llvm::Module> module,
                   const std::filesystem::path&  source)
    : context{context}
    , builder{builder}
    , module{module}
    , source{source}
  {
  }

  auto operator()(ast::nil) const -> llvm::Function*
  {
    BOOST_ASSERT(0);
  }

  auto operator()(const ast::function_decl& node) const -> llvm::Function*
  {
    auto* func_type = llvm::FunctionType::get(builder.getInt32Ty(), false);

    auto* func = llvm::Function::Create(func_type,
                                        llvm::Function::ExternalLinkage,
                                        node.name,
                                        module.get());

    return func;
  }

  auto operator()(const ast::function_def& node) const -> llvm::Function*
  {
    auto* func = module->getFunction(node.decl.name);

    if (!func)
      func = this->operator()(node.decl);

    if (!func) {
      throw std::runtime_error{format_error_message(
        source.string(),
        format("Failed to create function %s", node.decl.name),
        true)};
    }

    auto* basic_block = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(basic_block);

    for (auto&& statement : node.body)
      boost::apply_visitor(statement_visitor{builder, module, source},
                           statement);

    std::string              em;
    llvm::raw_string_ostream os{em};
    if (llvm::verifyFunction(*func, &os)) {
      func->eraseFromParent();

      throw std::runtime_error{format_error_message(source.string(), os.str())};
    }

    return func;
  }

private:
  llvm::LLVMContext&            context;
  llvm::IRBuilder<>&            builder;
  std::shared_ptr<llvm::Module> module;

  const std::filesystem::path& source;
};

code_generator::code_generator(const ast::program&          ast,
                               const std::filesystem::path& source)
  : builder{context}
  , module{std::make_shared<llvm::Module>(source.filename().string(), context)}
  , source{source}
  , ast{ast}
{
  codegen();
}

auto code_generator::write_llvm_ir_to_file(
  const std::filesystem::path& out) const -> void
{
  std::error_code      ostream_ec;
  llvm::raw_fd_ostream os{out.string(),
                          ostream_ec,
                          llvm::sys::fs::OpenFlags::OF_None};

  if (ostream_ec) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("%s: %s", out.string(), ostream_ec.message()))};
  }

  module->print(os, nullptr);
}

auto code_generator::write_object_code_to_file(
  const std::filesystem::path& out) const -> void
{
  const auto target_triple = llvm::sys::getDefaultTargetTriple();

  // target triple error string.
  std::string target_triple_es;
  auto        target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_es);

  if (!target) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("Failed to lookup target %s: %s", target_triple, target_triple_es),
      true)};
  }

  llvm::TargetOptions opt;
  auto*               the_target_machine
    = target->createTargetMachine(target_triple,
                                  "generic",
                                  "",
                                  opt,
                                  llvm::Optional<llvm::Reloc::Model>());

  module->setTargetTriple(target_triple);
  module->setDataLayout(the_target_machine->createDataLayout());

  std::error_code      ostream_ec;
  llvm::raw_fd_ostream os{out.string(),
                          ostream_ec,
                          llvm::sys::fs::OpenFlags::OF_None};
  if (ostream_ec) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("%s: %s\n", out.string(), ostream_ec.message()))};
  }

  llvm::legacy::PassManager pass;
  if (the_target_machine->addPassesToEmitFile(pass,
                                              os,
                                              nullptr,
                                              llvm::CGFT_ObjectFile)) {
    throw std::runtime_error{
      format_error_message("mikoc",
                           "TargetMachine can't emit a file of this types",
                           true)};
  }

  pass.run(*module);
  os.flush();
}

auto code_generator::codegen() -> void
{
  for (auto&& r : ast)
    boost::apply_visitor(toplevel_visitor{context, builder, module, source}, r);
}

} // namespace miko::codegen
