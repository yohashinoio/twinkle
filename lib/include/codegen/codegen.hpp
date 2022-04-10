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

namespace maple::codegen
{

struct LLVMTypeWithSign {
  llvm::Type* type;
  bool        is_signed;
};

// A structure that summarizes variables commonly needed by visitors.
struct CodegenContext {
  CodegenContext(const std::filesystem::path& file,
                 const PositionCache&         positions);

  [[nodiscard]] llvm::Value* int1_to_bool(llvm::Value* value);

  [[nodiscard]] std::string
  format_error(const boost::iterator_range<InputIterator> pos,
               const std::string_view                     message,
               const bool                                 with_code = true);

  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module>      module;
  llvm::IRBuilder<>                  builder;

  std::filesystem::path file;

  const PositionCache& positions;
};

struct CodeGenerator {
  CodeGenerator(const std::string_view       program_name,
                const ast::Program&          ast,
                const PositionCache&         positions,
                const std::filesystem::path& file_path,
                const bool                   opt,
                const llvm::Reloc::Model     relocation_model);

  void write_llvm_ir_to_file(const std::filesystem::path& path) const;

  void write_object_code_to_file(const std::filesystem::path& path);

  void write_assembly_to_file(const std::filesystem::path& path);

  // Returns the return value from the main function.
  [[nodiscard]] int jit_compile();

private:
  void codegen();

  void emit_file(const std::filesystem::path& path,
                 const llvm::CodeGenFileType  cgft);

  const std::string_view program_name;

  CodegenContext common;

  llvm::TargetMachine* target_machine;

  llvm::legacy::FunctionPassManager fp_manager;

  const ast::Program& ast;
};

} // namespace maple::codegen

#endif
