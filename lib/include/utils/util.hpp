/**
 * util.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _d63c66d6_93b5_11ec_b909_0242ac120002
#define _d63c66d6_93b5_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>

namespace maple
{

//===----------------------------------------------------------------------===//
// Helper
//===----------------------------------------------------------------------===//

namespace program_options = boost::program_options;

#define COLOR_DEFAULT "\x1b[0m"
#define COLOR_RED     "\x1b[91m"

constexpr unsigned int MAPLE_VER = 100000;

[[nodiscard]] std::string getVersion();

// With filename.
[[nodiscard]] std::string formatErrorMessage(const std::string_view filename,
                                             const std::string_view message,
                                             const bool fatal = false);

// Without filename.
[[nodiscard]] std::string formatErrorMessage(const std::string_view message,
                                             const bool fatal = false);

// Load a file to std::string.
[[nodiscard]] std::string loadFile(const std::string_view       program_name,
                                   const std::filesystem::path& path);

[[nodiscard]] program_options::options_description createOptionsDesc();

[[nodiscard]] program_options::variables_map
getVariableMap(const program_options::options_description& desc,
               const int                                   argc,
               const char* const* const                    argv);

// If no input files are passed, std::runtime_error is thrown.
[[nodiscard]] std::vector<std::string>
getInputFiles(const std::string_view                program_name,
              const program_options::variables_map& vmap);

[[nodiscard]] std::string stringToLower(const std::string_view str);

[[nodiscard]] llvm::Reloc::Model
getRelocationModel(const std::string_view                program_name,
                   const program_options::variables_map& vmap);

[[noreturn]] void unreachableInternal(const std::size_t line, const char* file);

#define unreachable() ::maple::unreachableInternal(__LINE__, __FILE__)

} // namespace maple

#endif
