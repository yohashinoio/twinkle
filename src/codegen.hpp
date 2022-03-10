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

#include "pch.hpp"
#include "ast.hpp"
#include "utility.hpp"

namespace miko::codegen
{

// A structure that summarizes variables commonly needed by visitors.
struct codegen_common {
  codegen_common(const std::filesystem::path& file);

  llvm::LLVMContext             context;
  std::shared_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  std::filesystem::path file;
};

struct code_generator {
  code_generator(const ast::program&          ast,
                 const position_cache&        positions,
                 const std::filesystem::path& file_path,
                 const bool                   optimize);

  void write_llvm_ir_to_file(const std::filesystem::path& out) const;

  void write_object_code_to_file(const std::filesystem::path& out) const;

private:
  void codegen();

  codegen_common common;

  llvm::TargetMachine* target_machine;

  llvm::legacy::FunctionPassManager function_pm;

  const ast::program&   ast;
  const position_cache& positions;
};

} // namespace miko::codegen

#endif
