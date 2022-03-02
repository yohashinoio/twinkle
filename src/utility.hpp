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

#define COLOR_DEFAULT "\x1b[0m"
#define COLOR_WHITE   "\x1b[97m"
#define COLOR_RED     "\x1b[91m"

constexpr const char* MIKO_VERSION = "1.0.0";

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

auto setup_program_options(boost::program_options::options_description& opt)
  -> void;

} // namespace miko

#endif
