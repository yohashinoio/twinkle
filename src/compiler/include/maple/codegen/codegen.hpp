/**
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

#include <maple/pch/pch.hpp>
#include <maple/ast/ast.hpp>
#include <maple/support/utils.hpp>
#include <maple/support/typedef.hpp>
#include <maple/jit/jit.hpp>
#include <maple/parse/parser.hpp>

namespace maple::codegen
{

// Function return type table.
struct FRTTable {
  // Regist stands for register.
  void regist(const std::string& name, const std::shared_ptr<Type> v)
  {
    function_return_tys.insert({name, v});
  }

  [[nodiscard]] auto begin() const noexcept
  {
    return function_return_tys.begin();
  }

  [[nodiscard]] auto end() const noexcept
  {
    return function_return_tys.end();
  }

  [[nodiscard]] std::optional<std::shared_ptr<Type>>
  operator[](const std::string& name) const noexcept
  {
    const auto it = function_return_tys.find(name);
    if (it == end())
      return std::nullopt;

    return it->second;
  }

private:
  std::unordered_map<std::string, std::shared_ptr<Type>> function_return_tys;
};

// User defined type table.
struct UDTTable {
  [[nodiscard]] std::optional<std::shared_ptr<Type>>
  operator[](const std::string& name) noexcept
  try {
    return user_defined_tys.at(name);
  }
  catch (const std::out_of_range&) {
    return std::nullopt;
  }

  // Regist stands for register.
  void regist(const std::string& name, const std::shared_ptr<Type> type)
  {
    user_defined_tys.emplace(name, type);
  }

  [[nodiscard]] bool exists(const std::string& name) const
  {
    return user_defined_tys.contains(name);
  }

private:
  // User defined types.
  std::unordered_map<std::string, std::shared_ptr<Type>> user_defined_tys;
};

// Codegen context.
struct CGContext {
  CGContext(llvm::LLVMContext&      context,
            PositionCache&&         positions,
            std::filesystem::path&& file) noexcept;

  [[nodiscard]] std::string
  formatError(const boost::iterator_range<InputIterator>& pos,
              const std::string_view                      message,
              const bool print_location = true) const;

  llvm::LLVMContext&            context;
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  std::filesystem::path file;

  PositionCache positions;

  UDTTable udt_table;

  FRTTable frt_table;
};

struct CodeGenerator {
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
               CGContext&                         ctx,
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
