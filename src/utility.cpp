//
//  utility.cpp
//
//  Copyright (c) 2022 The Miko Authors.
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
[[nodiscard]] std::string format_error_message(const std::string_view filename,
                                               const std::string_view message,
                                               const bool             fatal)
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
[[nodiscard]] std::string
format_error_message_without_filename(const std::string_view message,
                                      const bool             fatal)
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
[[nodiscard]] std::string load_file_to_string(const std::filesystem::path& path)
{
  if (!std::filesystem::exists(path)) {
    throw std::runtime_error{format_error_message(
      "mikoc",
      format("%s: No such file or directory", path.string()))};
  }

  if (auto file = std::ifstream{path}) {
    std::stringstream ss;
    ss << file.rdbuf();
    // NRVO
    return ss.str();
  }

  throw std::runtime_error{
    format_error_message("mikoc",
                         format("%s: Could not open file", path.string()))};
}

program_options::options_description create_options_description()
{
  program_options::options_description desc{"Options"};

  // clang-format off
  desc.add_options()
    ("help,h", "Display this information.")
    ("version,v", "Display compiler version.")
    ("input", program_options::value<std::string>(), "Input instead of files.")
    ("llvmir", "Output LLVM IR instead of Object code.")
    ("input-file", program_options::value<std::vector<std::string> >(), "Input file.")
    ;
  // clang-format on

  return desc;
}

program_options::variables_map
get_variable_map(const program_options::options_description& desc,
                 const int                                   argc,
                 const char* const* const                    argv)
{
  program_options::positional_options_description p;

  p.add("input-file", -1);

  program_options::variables_map vm;
  program_options::store(program_options::command_line_parser(argc, argv)
                           .options(desc)
                           .positional(p)
                           .run(),
                         vm);
  program_options::notify(vm);

  return vm;
}

std::vector<std::string>
get_input_files(const program_options::variables_map& vm)
{
  if (vm.count("input-file"))
    return vm["input-file"].as<std::vector<std::string>>();
  else {
    throw std::runtime_error{
      miko::format_error_message("mikoc", "no input files", true)};
  }
}

void display_version()
{
  const auto major = MIKO_VERSION / 100000;
  const auto minor = MIKO_VERSION / 100 % 1000;
  const auto patch = MIKO_VERSION % 100;

  std::cout << major << '.' << minor << '.' << patch << '\n';
}

} // namespace miko
