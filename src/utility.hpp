//
//  utility.hpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#ifndef _d63c66d6_93b5_11ec_b909_0242ac120002
#define _d63c66d6_93b5_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "pch.hpp"

namespace miko
{

namespace program_options = boost::program_options;

#define COLOR_DEFAULT "\x1b[0m"
#define COLOR_RED     "\x1b[91m"

constexpr unsigned int MIKO_VERSION = 100000;

auto display_version() -> void;

// Formatting and coloring.
[[nodiscard]] auto format_error_message(const std::string_view filename,
                                        const std::string_view message,
                                        const bool             fatal = false)
  -> std::string;

[[nodiscard]] auto
format_error_message_without_filename(const std::string_view message,
                                      const bool fatal = false) -> std::string;

// Load a file to std::string.
[[nodiscard]] auto load_file_to_string(const std::filesystem::path& path)
  -> std::string;

auto create_options_description() -> program_options::options_description;

auto get_variable_map(const program_options::options_description& desc,
                      const int                                   argc,
                      const char* const* const                    argv)
  -> program_options::variables_map;

// If no input files are passed, std::runtime_error is thrown.
auto get_input_files(const program_options::variables_map& vm)
  -> std::vector<std::string>;

} // namespace miko

#endif
