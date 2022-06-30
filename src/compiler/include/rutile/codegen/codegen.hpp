/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7d00490e_93b3_11ec_b909_0242ac120002
#define _7d00490e_93b3_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <rutile/pch/pch.hpp>
#include <rutile/ast/ast.hpp>
#include <rutile/codegen/type.hpp>
#include <rutile/support/utils.hpp>
#include <rutile/support/typedef.hpp>
#include <rutile/jit/jit.hpp>
#include <rutile/parse/parser.hpp>
#include <rutile/mangle/mangler.hpp>

namespace rutile::codegen
{

/*
  'Key' is the key type of the table
  'T' is the value type of the table
  'R' is the return type of operator[] (std::optional<R>)
  'R' must be able to return from const function
*/
template <typename Key, typename T, typename R = T>
struct Table {
  template <typename T1>
  [[nodiscard]] std::optional<R> operator[](T1&& key) const noexcept
  {
    const auto iter = table.find(std::forward<T1>(key));

    if (iter == end())
      return std::nullopt;
    else
      return iter->second;

    unreachable();
  }

  template <typename T1, typename T2>
  void insert(T1&& key, T2&& value)
  {
    assert(!table.contains(std::forward<T1>(key)));

    table.emplace(std::forward<T1>(key), std::forward<T2>(value));
  }

  template <typename T1, typename T2>
  void insertOrAssign(T1&& key, T2&& value)
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

  [[nodiscard]] bool empty() const noexcept
  {
    return table.empty();
  }

private:
  std::unordered_map<Key, T> table;
};

using FunctionReturnTypeTable = Table<llvm::Function*, std::shared_ptr<Type>>;

using AliasTable = Table<std::string, std::shared_ptr<Type>>;

using ClassTable = Table<std::string, std::shared_ptr<ClassType>>;

enum class NamespaceKind {
  unknown,
  namespace_,
  class_,
};

struct Namespace {
  Namespace(std::string&& name, const NamespaceKind kind)
    : name{std::move(name)}
    , kind{kind}
  {
  }

  Namespace(const std::string& name, const NamespaceKind kind)
    : name{std::move(name)}
    , kind{kind}
  {
  }

  const std::string   name;
  const NamespaceKind kind;
};

// Namespace hierarchy
struct NsHierarchy {
  [[nodiscard]] bool empty() const noexcept
  {
    return namespaces.empty();
  }

  void push(const Namespace& n)
  {
    namespaces.push_back(n);
  }

  void pop()
  {
    namespaces.pop_back();
  }

  [[nodiscard]] const Namespace& top() const
  {
    assert(!empty());
    return namespaces.back();
  }

  decltype(auto) begin() const noexcept
  {
    return namespaces.begin();
  }

  decltype(auto) end() const noexcept
  {
    return namespaces.end();
  }

private:
  std::deque<Namespace> namespaces;
};

// Codegen context.
struct CGContext : private boost::noncopyable {
  CGContext(llvm::LLVMContext&      context,
            PositionCache&&         positions,
            std::filesystem::path&& file,
            const std::string&      source_code) noexcept;

  [[nodiscard]] std::string
  formatError(const boost::iterator_range<InputIterator>& pos,
              const std::string_view                      message) const;

  // LLVM
  llvm::LLVMContext&            context;
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  std::filesystem::path file;

  const PositionCache positions;

  // Table
  ClassTable              class_table;
  FunctionReturnTypeTable return_type_table;
  AliasTable              alias_table;

  NsHierarchy ns_hierarchy;

  // Mangle
  mangle::Mangler mangler;

private:
  // Stores source code line by line as elements.
  const std::vector<std::string> source_code;

  [[nodiscard]] std::size_t
  calcRows(const boost::iterator_range<InputIterator>& pos) const;
};

struct CodeGenerator : private boost::noncopyable {
  CodeGenerator(const std::string_view               program_name,
                std::vector<parse::Parser::Result>&& parse_results,
                const unsigned int                   opt_level,
                const llvm::Reloc::Model             relocation_model);

  void emitLlvmIRFiles();

  void emitObjectFiles();

  void emitAssemblyFiles();

  // Returns the return value from the main function.
  [[nodiscard]] int doJIT();

private:
  void verifyOptLevel(const unsigned int opt_level) const;

  using Result
    = std::tuple<std::unique_ptr<llvm::Module>, std::filesystem::path>;

  void codegen(const ast::TranslationUnit&        ast,
               CGContext&                         ctx,
               llvm::legacy::FunctionPassManager& fpm);

  void emitFiles(const llvm::CodeGenFileType cgft);

  void initTargetTripleAndMachine();

  const std::string_view argv_front;

  std::unique_ptr<llvm::LLVMContext> context;

  bool jit_compiled = false;

  std::string          target_triple;
  llvm::TargetMachine* target_machine;

  const llvm::Reloc::Model relocation_model;

  std::vector<Result> results;

  std::vector<parse::Parser::Result> parse_results;
};

} // namespace rutile::codegen

#endif
