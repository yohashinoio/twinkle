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

namespace miko
{

//===----------------------------------------------------------------------===//
// Helper
//===----------------------------------------------------------------------===//

namespace program_options = boost::program_options;

#define COLOR_DEFAULT "\x1b[0m"
#define COLOR_RED     "\x1b[91m"

constexpr unsigned int MIKO_VERSION = 100000;

void display_version();

// Formatting and coloring.
[[nodiscard]] std::string format_error_message(const std::string_view filename,
                                               const std::string_view message,
                                               const bool fatal = false);

// Formatting and coloring.
[[nodiscard]] std::string
format_error_message_without_filename(const std::string_view message,
                                      const bool             fatal = false);

// Load a file to std::string.
[[nodiscard]] std::string
load_file_to_string(const std::string_view       program_name,
                    const std::filesystem::path& path);

[[nodiscard]] program_options::options_description create_options_description();

[[nodiscard]] program_options::variables_map
get_variable_map(const program_options::options_description& desc,
                 const int                                   argc,
                 const char* const* const                    argv);

// If no input files are passed, std::runtime_error is thrown.
[[nodiscard]] std::vector<std::string>
get_input_files(const std::string_view                program_name,
                const program_options::variables_map& vm);

[[noreturn]] void unreachable_internal(const std::size_t line,
                                       const char*       file);

#define unreachable() ::miko::unreachable_internal(__LINE__, __FILE__)

} // namespace miko

#endif
