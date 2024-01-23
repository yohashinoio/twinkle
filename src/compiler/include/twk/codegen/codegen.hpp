/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _7d00490e_93b3_11ec_b909_0242ac120002
#define _7d00490e_93b3_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twk/pch/pch.hpp>
#include <twk/ast/ast.hpp>
#include <twk/codegen/type.hpp>
#include <twk/support/utils.hpp>
#include <twk/support/typedef.hpp>
#include <twk/jit/jit.hpp>
#include <twk/parse/parser.hpp>
#include <twk/mangle/mangler.hpp>
#include <map>

namespace twk
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
  [[nodiscard]] std::optional<const std::reference_wrapper<const R>>
  operator[](T1&& key) const noexcept
  {
    const auto iter = table.find(std::forward<T1>(key));

    if (iter == end())
      return std::nullopt;
    else
      return std::ref(iter->second);

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

using FunctionParameterTypesTable
  = Table<llvm::Function*, std::vector<std::shared_ptr<Type>>>;

using TypeTable = Table<std::string, std::shared_ptr<Type>>;

using AliasTable = TypeTable;

using TemplateArgumentTable = TypeTable;

using ClassTable = Table<std::string, std::shared_ptr<ClassType>>;

struct Variable;
using SymbolTable = Table<std::string, std::shared_ptr<Variable>>;

using UnionTable = Table<std::string, std::shared_ptr<UnionType>>;

using PositionCacheTable = Table<std::string /* file name */, PositionCache>;

// Stores source code line by line as elements
using SourceCode = std::vector<std::string>;

using SourceCodeTable = Table<std::string /* file name */, SourceCode>;

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

struct NamespaceStack {
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

  [[nodiscard]] bool contains(const std::string_view x) const
  {
    for (const auto& r : *this) {
      if (r.name == x)
        return true;
    }

    return false;
  }

  // Implemented to be a key in std::map
  [[nodiscard]] bool operator<(const NamespaceStack& other) const
  {
    return std::lexicographical_compare(namespaces.begin(),
                                        namespaces.end(),
                                        other.namespaces.begin(),
                                        other.namespaces.end());
  }

private:
  std::deque<Namespace> namespaces;
};

using TemplateTableKey = std::tuple<std::string, // Name
                                    std::size_t, // Template parameter length
                                    NamespaceStack>;

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

using CreatedClassTemplateTableKey = std::tuple<std::string, // Name
                                                ast::TemplateArguments,
                                                NamespaceStack>;

using CreatedClassTemplateTableElem
  = std::pair<CreatedClassTemplateTableKey, std::shared_ptr<Type>>;

// std::unordered_map cannot use std::tuple as a key, so use std::map instead
using UnionTemplateTable = Table<TemplateTableKey,
                                 ast::UnionDef,
                                 std::map<TemplateTableKey, ast::UnionDef>>;

struct CreatedClassTemplateTable {
  CreatedClassTemplateTable(CGContext& ctx)
    : ctx{ctx}
    , table{}
  {
  }

  [[nodiscard]] std::optional<std::shared_ptr<Type>>
  operator[](const CreatedClassTemplateTableKey& key) const;

  void insert(CreatedClassTemplateTableKey&& key,
              const std::shared_ptr<Type>&   value);

private:
  CGContext&                              ctx;
  std::set<CreatedClassTemplateTableElem> table;
};

template <typename T>
concept PositionTaggedClass
  = std::is_convertible_v<T, boost::spirit::x3::position_tagged>;

// Codegen context
struct CGContext : private boost::noncopyable {
  CGContext(llvm::LLVMContext&      context,
            PositionCache&&         current_file_poscache,
            std::filesystem::path&& file,
            const std::string&      source_code,
            const unsigned int      opt_level,
            const bool              jit) noexcept;

  [[nodiscard]] std::string
  formatError(const boost::iterator_range<InputIterator>& pos,
              const std::string_view                      message) const;

  // LLVM
  llvm::LLVMContext&            context;
  std::unique_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  std::filesystem::path current_file;

  template <PositionTaggedClass T>
  [[nodiscard]] PositionRange positionOf(T&& ast) const
  {
    const auto search_in_current_file = [&]() {
      const auto current_file_pos_cache
        = position_cache_table[current_file.string()];
      assert(current_file_pos_cache);
      return current_file_pos_cache->get().position_of(ast);
    };

    const auto search_in_imported_files
      = [&]() -> std::optional<PositionRange> {
      const auto current_file_name = current_file.string();

      for (const auto& r : position_cache_table) {
        if (r.first == current_file_name)
          continue;

        try {
          return r.second.position_of(ast);
        }
        catch (const std::out_of_range&) {
          continue;
        }
      }

      return std::nullopt;
    };

    try {
      return search_in_current_file();
    }
    catch (const std::out_of_range&) {
      const auto pos = search_in_imported_files();
      if (pos)
        return *pos;
      else
        unreachable();
    }
  }

  [[nodiscard]] const PositionCache& getCurrentPosCache() const
  {
    const auto current_pos_cache = position_cache_table[current_file.string()];
    assert(current_pos_cache);
    return *current_pos_cache;
  }

  // Table
  ClassTable                        class_table;
  FunctionReturnTypeTable           return_type_table;
  FunctionParameterTypesTable       param_types_table;
  FunctionTemplateTable             func_template_table;
  AliasTable                        alias_table;
  ClassTemplateTable                class_template_table;
  CreatedClassTemplateTable         created_class_template_table;
  UnionTable                        union_table;
  UnionTemplateTable                union_template_table;
  PositionCacheTable                position_cache_table;
  // If you want to find template arguments, look for them in the symbol table
  // of top
  std::stack<TemplateArgumentTable> template_argument_tables;

  // Namespace
  NamespaceStack ns_hierarchy;

  // Mangle
  mangle::Mangler mangler;

  // Pass manager
  llvm::legacy::FunctionPassManager fpm;

  // If true, suppress optimization
  const bool jit;

private:
  SourceCodeTable source_code_table;

  [[nodiscard]] std::size_t
  calcRows(const boost::iterator_range<InputIterator>& pos) const;
};

struct CodeGenerator : private boost::noncopyable {
  CodeGenerator(const std::string_view               program_name,
                std::vector<parse::Parser::Result>&& parse_results,
                const unsigned int                   opt_level,
                const llvm::Reloc::Model             relocation_model,
                const std::optional<std::string>&    target_triple_arg,
                const bool                           jit);

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

  void initTargetTripleAndMachine(
    const std::optional<std::string>& target_triple_arg);

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

} // namespace twk

#endif
