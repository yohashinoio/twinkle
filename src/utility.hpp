//
//  utility.hpp
//
//  Copyright (c) 2022 The Miko Authors.
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

using input_iterator_type = std::string::const_iterator;
using position_cache
  = boost::spirit::x3::position_cache<std::vector<input_iterator_type>>;

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
load_file_to_string(const std::filesystem::path& path);

program_options::options_description create_options_description();

program_options::variables_map
get_variable_map(const program_options::options_description& desc,
                 const int                                   argc,
                 const char* const* const                    argv);

// If no input files are passed, std::runtime_error is thrown.
std::vector<std::string>
get_input_files(const program_options::variables_map& vm);

} // namespace miko

#endif
