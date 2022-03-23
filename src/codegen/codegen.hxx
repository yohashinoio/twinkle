/**
 * codegen.hxx
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

#include <pch/pch.hxx>
#include <ast/ast.hxx>
#include <utils/util.hxx>
#include <utils/typedef.hxx>
#include <jit/jit.hxx>

namespace miko::codegen
{

struct llvm_type_info {
  llvm::Type* type;
  bool        is_signed;
};

// A structure that summarizes variables commonly needed by visitors.
struct codegen_common {
  codegen_common(const std::filesystem::path& file,
                 const position_cache&        positions);

  [[nodiscard]] llvm_type_info typename_to_type(const id::type_name type,
                                                const bool is_ptr = false);

  [[nodiscard]] llvm::Value* i1_to_boolean(llvm::Value* value);

  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module>      module;
  llvm::IRBuilder<>                  builder;

  std::filesystem::path file;

  const position_cache& positions;
};

struct code_generator {
  code_generator(const std::string_view       program_name,
                 const ast::program&          ast,
                 const position_cache&        positions,
                 const std::filesystem::path& file_path,
                 const bool                   optimize);

  void write_llvm_ir_to_file(const std::filesystem::path& out) const;

  void write_object_code_to_file(const std::filesystem::path& out);

  // Returns the return value from the main function.
  int jit_compile();

private:
  void codegen();

  const std::string_view program_name;

  codegen_common common;

  llvm::TargetMachine* target_machine;

  llvm::legacy::FunctionPassManager function_pm;

  const ast::program& ast;
};

} // namespace miko::codegen

#endif
