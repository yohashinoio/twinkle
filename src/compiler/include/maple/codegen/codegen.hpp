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

/*
  'Key' is the key type of the table.
  'T' is the value type of the table.
  'R' is the return type of operator[]. (std::optional<R>)
*/
template <typename Key, typename T, typename R = T>
struct Table {
  template <typename T1>
  [[nodiscard]] std::optional<R> operator[](T1&& key) noexcept
  {
    const auto iter = table.find(std::forward<T1>(key));

    if (iter == end())
      return std::nullopt;
    else
      return iter->second;

    unreachable();
  }

  template <typename T1, typename T2>
  void regist(T1&& key, T2&& value)
  {
    assert(!table.contains(std::forward<T1>(key)));
    table.emplace(std::forward<T1>(key), std::forward<T2>(value));
  }

  template <typename T1, typename T2>
  void registOrOverwrite(T1&& key, T2&& value)
  {
    table.insert_or_assign(std::forward<T1>(key), std::forward<T2>(value));
  }

  auto begin() const noexcept
  {
    return table.begin();
  }

  auto end() const noexcept
  {
    return table.end();
  }

  template <typename T1>
  [[nodiscard]] bool exists(T1&& key) const
  {
    return table.contains(std::forward<T1>(key));
  }

private:
  std::unordered_map<Key, T> table;
};

// Function return type table.
using FRTTable = Table<std::string, std::shared_ptr<Type>>;

using StructTypeWithSignInfo
  = std::pair<llvm::StructType*, std::deque<SignKind> /* Sign info */>;

using StructTable = Table<std::string, StructTypeWithSignInfo>;

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

  StructTable struct_table;

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
