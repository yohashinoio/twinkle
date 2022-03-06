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

struct symbol_table {
  llvm::AllocaInst* operator[](const std::string& name) const noexcept
  try {
    return named_values.at(name);
  }
  catch (const std::out_of_range&) {
    return nullptr;
  }

  void insert(const std::string& name, llvm::AllocaInst* value)
  {
    named_values.insert({name, value});
  }

private:
  std::unordered_map<std::string, llvm::AllocaInst*> named_values;
};

// Create an alloca instruction in the entry block of
// the function.  This is used for mutable variables etc.
llvm::AllocaInst* create_entry_block_alloca(llvm::Function*    func,
                                            llvm::LLVMContext& context,
                                            const std::string& var_name)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());

  return tmp.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, var_name);
}

// It seems that the member function has to be const.
struct expression_visitor : public boost::static_visitor<llvm::Value*> {
  expression_visitor(llvm::IRBuilder<>&            builder,
                     std::shared_ptr<llvm::Module> module,
                     symbol_table&                 named_values,
                     const std::filesystem::path&  source)
    : builder{builder}
    , module{module}
    , named_values{named_values}
    , source{source}
  {
  }

  llvm::Value* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  llvm::Value* operator()(const int value) const
  {
    return llvm::ConstantInt::get(builder.getInt32Ty(), value);
  }

  llvm::Value* operator()(const ast::unaryop& node) const
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

  llvm::Value* operator()(const ast::binop& node) const
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

  llvm::Value* operator()(const ast::variable& node) const
  {
    auto* ainst = named_values[node.name];
    if (!ainst) {
      throw std::runtime_error{format_error_message(
        source.string(),
        format("Unknown variable '%s' referenced", node.name))};
    }

    return builder.CreateLoad(ainst->getAllocatedType(),
                              ainst,
                              node.name.c_str());
  }

  llvm::Value* operator()(const ast::function_call& node) const
  {
    auto* callee_f = module->getFunction(node.callee);

    if (!callee_f) {
      throw std::runtime_error{format_error_message(
        source.string(),
        format("Unknown function '%s' referenced", node.callee))};
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

  symbol_table& named_values;

  const std::filesystem::path& source;

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

struct statement_visitor : public boost::static_visitor<void> {
  statement_visitor(llvm::IRBuilder<>&            builder,
                    std::shared_ptr<llvm::Module> module,
                    symbol_table&                 named_values,
                    const std::filesystem::path&  source)
    : builder{builder}
    , module{module}
    , named_values{named_values}
    , source{source}
  {
  }

  void operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  void operator()(const ast::expression& node) const
  {
    boost::apply_visitor(
      expression_visitor{builder, module, named_values, source},
      node);
  }

  void operator()(const ast::return_statement& node) const
  {
    auto* retval = boost::apply_visitor(
      expression_visitor{builder, module, named_values, source},
      node.rhs);

    builder.CreateRet(retval);
  }

private:
  llvm::IRBuilder<>&            builder;
  std::shared_ptr<llvm::Module> module;

  symbol_table& named_values;

  const std::filesystem::path& source;
};

struct top_visitor : public boost::static_visitor<llvm::Function*> {
  top_visitor(llvm::LLVMContext&                 context,
              llvm::IRBuilder<>&                 builder,
              std::shared_ptr<llvm::Module>      module,
              llvm::legacy::FunctionPassManager& fpm,
              const std::filesystem::path&       source)
    : context{context}
    , builder{builder}
    , module{module}
    , fpm{fpm}
    , source{source}
  {
  }

  llvm::Function* operator()(ast::nil) const
  {
    BOOST_ASSERT(0);
  }

  llvm::Function* operator()(const ast::function_decl& node) const
  {
    auto* func_type = llvm::FunctionType::get(builder.getInt32Ty(), false);

    auto* func = llvm::Function::Create(func_type,
                                        llvm::Function::ExternalLinkage,
                                        node.name,
                                        module.get());

    return func;
  }

  llvm::Function* operator()(const ast::function_def& node) const
  {
    symbol_table named_values;

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
      boost::apply_visitor(
        statement_visitor{builder, module, named_values, source},
        statement);

    std::string              em;
    llvm::raw_string_ostream os{em};
    if (llvm::verifyFunction(*func, &os)) {
      func->eraseFromParent();

      throw std::runtime_error{format_error_message(source.string(), os.str())};
    }

    fpm.run(*func);

    return func;
  }

private:
  llvm::LLVMContext&            context;
  llvm::IRBuilder<>&            builder;
  std::shared_ptr<llvm::Module> module;

  llvm::legacy::FunctionPassManager& fpm;

  const std::filesystem::path& source;
};

code_generator::code_generator(const ast::program&          ast,
                               const std::filesystem::path& source)
  : builder{context}
  , module{std::make_shared<llvm::Module>(source.filename().string(), context)}
  , fpm{module.get()}
  , source{source}
  , ast{ast}
{
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  fpm.add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  fpm.add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
  fpm.add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  fpm.add(llvm::createCFGSimplificationPass());

  fpm.doInitialization();

  codegen();
}

void code_generator::write_llvm_ir_to_file(
  const std::filesystem::path& out) const
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

void code_generator::write_object_code_to_file(
  const std::filesystem::path& out) const
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

  llvm::legacy::PassManager pm;
  if (the_target_machine->addPassesToEmitFile(pm,
                                              os,
                                              nullptr,
                                              llvm::CGFT_ObjectFile)) {
    throw std::runtime_error{
      format_error_message("mikoc",
                           "TargetMachine can't emit a file of this types",
                           true)};
  }

  pm.run(*module);
  os.flush();
}

void code_generator::codegen()
{
  for (auto&& node : ast)
    boost::apply_visitor(top_visitor{context, builder, module, fpm, source},
                         node);
}

} // namespace miko::codegen
