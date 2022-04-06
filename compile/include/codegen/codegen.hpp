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

namespace miko::codegen
{

struct LLVMTypeWithSign {
  llvm::Type* type;
  bool        is_signed;
};

// A structure that summarizes variables commonly needed by visitors.
struct CodegenContext {
  CodegenContext(const std::filesystem::path& file,
                 const PositionCache&         positions);

  [[nodiscard]] std::optional<LLVMTypeWithSign>
  typename_to_type(const id::TypeName type, const bool is_ptr = false);

  [[nodiscard]] llvm::Value* i1_to_boolean(llvm::Value* value);

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
                const bool                   optimize);

  void write_llvm_ir_to_file(const std::filesystem::path& out) const;

  void write_object_code_to_file(const std::filesystem::path& out);

  // Returns the return value from the main function.
  [[nodiscard]] int jit_compile();

private:
  void codegen();

  const std::string_view program_name;

  CodegenContext common;

  llvm::TargetMachine* target_machine;

  llvm::legacy::FunctionPassManager fpm;

  const ast::Program& ast;
};

} // namespace miko::codegen

#endif
