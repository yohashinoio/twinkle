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
#include <parse/parse_typedef.hpp>
#include <codegen/codegen_typedef.hpp>

namespace maple::codegen
{

struct CodegenContext {
  CodegenContext(llvm::LLVMContext&           context,
                 const std::filesystem::path& file,
                 const PositionCache&         positions);

  [[nodiscard]] llvm::Value* int1_to_bool(llvm::Value* value);

  [[nodiscard]] std::string
  format_error(const boost::iterator_range<InputIterator> pos,
               const std::string_view                     message,
               const bool                                 with_code = true);

  llvm::LLVMContext&            context;
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  std::filesystem::path file;

  const PositionCache& positions;
};

struct CodeGenerator {
  CodeGenerator(const std::string_view            program_name,
                std::vector<parse::ParseResult>&& ast,
                const bool                        opt,
                const llvm::Reloc::Model          relocation_model);

  void emit_llvmIR_files();

  void emit_object_files();

  void emit_assembly_files();

  // Returns the return value from the main function.
  [[nodiscard]] int do_JIT();

private:
  void codegen(const ast::Program&                ast,
               CodegenContext&                    ctx,
               llvm::legacy::FunctionPassManager& fp_manager);

  void emit_files(const llvm::CodeGenFileType cgft);

  void init_target_triple_and_machine();

  const std::string_view argv_front;

  std::unique_ptr<llvm::LLVMContext> context;

  bool jit_compiled = false;

  std::string          target_triple;
  llvm::TargetMachine* target_machine;

  llvm::Reloc::Model relocation_model;

  std::vector<CodegenResult> results;

  std::vector<parse::ParseResult> asts;
};

// Returns the return value from the main function.
[[nodiscard]] int do_JIT(std::vector<std::shared_ptr<llvm::Module>>&& modules);

} // namespace maple::codegen

#endif
