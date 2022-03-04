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
      return format("%s: " COLOR_RED "fatal error: " COLOR_DEFAULT "%s",
                    filename.data(),
                    message.data());
    }
    else {
      return format("%s: " COLOR_RED "error: " COLOR_DEFAULT "%s",
                    filename.data(),
                    message.data());
    }
  }
#endif
  if (fatal)
    return format("%s: fatal error: %s", filename.data(), message.data());

  return format("%s: error: %s", filename.data(), message.data());
}

// Formatting and coloring.
[[nodiscard]] auto
format_error_message_without_filename(const std::string_view message,
                                      const bool fatal) -> std::string
{
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    if (fatal)
      return format(COLOR_RED "fatal error: " COLOR_DEFAULT "%s",
                    message.data());
    else
      return format(COLOR_RED "error: " COLOR_DEFAULT "%s", message.data());
  }
#endif
  if (fatal)
    return format("fatal error: %s", message.data());

  return format("error: %s", message.data());
}

// Load a file to std::string.
[[nodiscard]] auto load_file_to_string(const std::filesystem::path& path)
  -> std::string
{
  if (!std::filesystem::exists(path)) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("%s: No such file or directory\n", path.string()))};
  }

  if (auto file = std::ifstream{path}) {
    std::stringstream ss;
    ss << file.rdbuf();
    // NRVO
    return ss.str();
  }

  throw std::runtime_error{
    format_error_message("mikoc",
                         format("%s: Could not open file\n", path.string()))};
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
  const auto major = MIKO_VERSION / 100000;
  const auto minor = MIKO_VERSION / 100 % 1000;
  const auto patch = MIKO_VERSION % 100;

  std::cout << major << '.' << minor << '.' << patch << '\n';
}

} // namespace miko
