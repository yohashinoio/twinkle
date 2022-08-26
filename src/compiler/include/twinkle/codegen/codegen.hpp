/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _7d00490e_93b3_11ec_b909_0242ac120002
#define _7d00490e_93b3_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>
#include <twinkle/ast/ast.hpp>
#include <twinkle/codegen/type.hpp>
#include <twinkle/support/utils.hpp>
#include <twinkle/support/typedef.hpp>
#include <twinkle/jit/jit.hpp>
#include <twinkle/parse/parser.hpp>
#include <twinkle/mangle/mangler.hpp>
#include <map>

namespace twinkle
{

using FilePaths = std::vector<std::filesystem::path>;

namespace codegen
{

/*
  'Key' is the key type of the table
  'T' is the value type of the table
  'R' is the return type of operator[] (std::optional<R>)
  'R' must be able to return from const function
*/
template <typename Key,
          typename T,
          typename MapT = std::unordered_map<Key, T>,
          typename R    = T>
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
    assert(!table.contains(key));

    table.emplace(std::forward<T1>(key), std::forward<T2>(value));
  }

  template <typename T1, typename T2>
  void insertOrAssign(T1&& key, T2&& value)
  {
    table.insert_or_assign(std::forward<T1>(key), std::forward<T2>(value));
  }

  template <typename T1>
  void erase(T1&& key)
  {
    table.erase(std::forward<T1>(key));
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
  MapT table;
};

using FunctionReturnTypeTable = Table<llvm::Function*, std::shared_ptr<Type>>;

using AliasTable = Table<std::string, std::shared_ptr<Type>>;

using ClassTable = Table<std::string, std::shared_ptr<ClassType>>;

struct Variable;
using SymbolTable = Table<std::string, std::shared_ptr<Variable>>;

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

  [[nodiscard]] bool operator<(const Namespace& other) const
  {
    return name < other.name;
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

  Namespace pop()
  {
    const auto tmp = top();
    namespaces.pop_back();
    return tmp;
  }

  [[nodiscard]] const Namespace& top() const
  {
    assert(!empty());
    return namespaces.back();
  }

  [[nodiscard]] decltype(auto) begin() const noexcept
  {
    return namespaces.begin();
  }

  [[nodiscard]] decltype(auto) end() const noexcept
  {
    return namespaces.end();
  }

  // Implemented to be a key in std::map
  [[nodiscard]] bool operator<(const NsHierarchy& other) const
  {
    return std::lexicographical_compare(namespaces.begin(),
                                        namespaces.end(),
                                        other.namespaces.begin(),
                                        other.namespaces.end());
  }

private:
  std::deque<Namespace> namespaces;
};

using TemplateTableKey = std::tuple<std::string, // Function name
                                    std::size_t, // Template parameter length
                                    NsHierarchy>;

using FunctionTemplateTableValue = ast::FunctionDef;

// std::unordered_map cannot use std::tuple as a key, so use std::map instead
using FunctionTemplateTable
  = Table<TemplateTableKey,
          FunctionTemplateTableValue,
          std::map<TemplateTableKey, FunctionTemplateTableValue>>;

using ClassTemplateTableValue = ast::ClassDef;

// std::unordered_map cannot use std::tuple as a key, so use std::map instead
using ClassTemplateTable
  = Table<TemplateTableKey,
          ClassTemplateTableValue,
          std::map<TemplateTableKey, ClassTemplateTableValue>>;

// Codegen context
struct CGContext : private boost::noncopyable {
  CGContext(llvm::LLVMContext&      context,
            PositionCache&&         positions,
            std::filesystem::path&& file,
            const std::string&      source_code,
            const unsigned int      opt_level) noexcept;

  [[nodiscard]] std::string
  formatError(const boost::iterator_range<InputIterator>& pos,
              const std::string_view                      message) const;

  // LLVM
  llvm::LLVMContext&            context;
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  std::filesystem::path file;

  PositionCache positions;

  // Table
  ClassTable              class_table;
  FunctionReturnTypeTable return_type_table;
  AliasTable              alias_table;
  FunctionTemplateTable   func_template_table;
  ClassTemplateTable      class_template_table;

  // Namespace
  NsHierarchy ns_hierarchy;

  // Mangle
  mangle::Mangler mangler;

  // Pass manager
  llvm::legacy::FunctionPassManager fpm;

private:
  // Stores source code line by line as elements
  const std::vector<std::string> source_code;

  [[nodiscard]] std::size_t
  calcRows(const boost::iterator_range<InputIterator>& pos) const;
};

struct CodeGenerator : private boost::noncopyable {
  CodeGenerator(const std::string_view               program_name,
                std::vector<parse::Parser::Result>&& parse_results,
                const unsigned int                   opt_level,
                const llvm::Reloc::Model             relocation_model);

  // Returns the created file paths
  [[nodiscard]] FilePaths emitLlvmIRFiles();

  // Returns the created file paths
  [[nodiscard]] FilePaths emitObjectFiles();

  // Returns the created file paths
  [[nodiscard]] FilePaths emitTemporaryObjectFiles();

  // Returns the created file paths
  [[nodiscard]] FilePaths emitAssemblyFiles();

  // Returns the return value from the main function
  [[nodiscard]] int doJIT();

private:
  void verifyOptLevel(const unsigned int opt_level) const;

  using Result
    = std::tuple<std::unique_ptr<llvm::Module>, std::filesystem::path>;

  void codegen(const ast::TranslationUnit& ast, CGContext& ctx);

  // Returns the created file paths
  [[nodiscard]] FilePaths emitFiles(const llvm::CodeGenFileType cgft,
                                    const bool create_as_tmpfile = false);

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

} // namespace codegen

} // namespace twinkle

#endif
