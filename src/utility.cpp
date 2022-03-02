//
//  utility.cpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#include "pch.hpp"
#include "utility.hpp"
#include <fstream>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace miko
{

// Formatting and coloring.
[[nodiscard]] auto format_error_message(const std::string_view filename,
                                        const std::string_view message,
                                        const bool fatal) -> std::string
{
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    if (fatal) {
      return format("%s: " COLOR_RED "Fatal error: " COLOR_WHITE
                    "%s" COLOR_DEFAULT,
                    filename.data(),
                    message.data());
    }
    else {
      return format("%s: " COLOR_RED "Error: " COLOR_WHITE "%s" COLOR_DEFAULT,
                    filename.data(),
                    message.data());
    }
  }
#endif
  if (fatal)
    return format("%s: Fatal error: %s", filename.data(), message.data());

  return format("%s: Error: %s", filename.data(), message.data());
}

// Formatting and coloring.
[[nodiscard]] auto
format_error_message_without_filename(const std::string_view message,
                                      const bool fatal) -> std::string
{
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    if (fatal)
      return format(COLOR_RED "Fatal error: " COLOR_WHITE "%s" COLOR_DEFAULT,
                    message.data());
    else
      return format(COLOR_RED "Error: " COLOR_WHITE "%s" COLOR_DEFAULT,
                    message.data());
  }
#endif
  if (fatal)
    return format("Fatal error: %s", message.data());

  return format("Error: %s", message.data());
}

// Load a file to std::string.
[[nodiscard]] auto load_file_to_string(const std::filesystem::path& path)
  -> std::string
{
  if (auto file = std::ifstream{path}) {
    std::stringstream ss;
    ss << file.rdbuf();
    // NRVO
    return ss.str();
  }

  throw std::runtime_error{format_error_message(
    path.string(),
    format("%s: No such file or directory.", path.string()),
    true)};
}

auto setup_program_options(boost::program_options::options_description& opt)
  -> void
{
  namespace program_options = boost::program_options;

  // clang-format off
 opt.add_options()
    ("help,h", "Display this information.")
    ("version,v", "Display compiler version information.")
    ("irprint", "Display LLVM IR.")
    ("out,o", program_options::value<std::string>(), "Specify output file name.")
    ;
  // clang-format on
}

auto display_version() -> void
{
  std::cout << MIKO_VERSION << std::endl;
}

} // namespace miko
