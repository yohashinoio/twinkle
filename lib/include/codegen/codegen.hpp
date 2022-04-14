/**
 * codegen.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7d00490e_93b3_11ec_b909_0242ac120002
#define _7d00490e_93b3_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <ast/ast.hpp>
#include <utils/util.hpp>
#include <utils/typedef.hpp>
#include <jit/jit.hpp>
#include <parse/parse.hpp>

namespace maple::codegen
{

struct CodeGenerator {
  struct Context {
    Context(llvm::LLVMContext&      context,
            PositionCache&&         positions,
            std::filesystem::path&& file) noexcept;

    [[nodiscard]] llvm::Value* int1ToBool(llvm::Value* value);

    [[nodiscard]] std::string
    formatError(const boost::iterator_range<InputIterator> pos,
                const std::string_view                     message,
                const bool                                 with_code = true);

    llvm::LLVMContext&            context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<>             builder;

    std::filesystem::path file;

    PositionCache positions;
  };

  CodeGenerator(const std::string_view               program_name,
                std::vector<parse::Parser::Result>&& ast,
                const bool                           opt,
                const llvm::Reloc::Model             relocation_model);

  void emitLlvmIRFiles();

  void emitObjectFiles();

  void emitAssemblyFiles();

  // Returns the return value from the main function.
  [[nodiscard]] int doJIT();

private:
  using Result
    = std::tuple<std::unique_ptr<llvm::Module>, std::filesystem::path>;

  void codegen(const ast::Program&                ast,
               Context&                           ctx,
               llvm::legacy::FunctionPassManager& fp_manager);

  void emitFiles(const llvm::CodeGenFileType cgft);

  void initTargetTripleAndMachine();

  const std::string_view argv_front;

  std::unique_ptr<llvm::LLVMContext> context;

  bool jit_compiled = false;

  std::string          target_triple;
  llvm::TargetMachine* target_machine;

  llvm::Reloc::Model relocation_model;

  std::vector<Result> results;

  std::vector<parse::Parser::Result> parse_results;
};

} // namespace maple::codegen

#endif
