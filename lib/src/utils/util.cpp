/**
 * util.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <pch/pch.hpp>
#include <utils/format.hpp>
#include <utils/util.hpp>
#include <fstream>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace maple
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
[[nodiscard]] std::string
load_file_to_string(const std::string_view       program_name,
                    const std::filesystem::path& path)
{
  if (!std::filesystem::exists(path)) {
    throw std::runtime_error{format_error_message(
      program_name,
      format("%s: No such file or directory", path.string()))};
  }

  if (auto file = std::ifstream{path}) {
    std::stringstream ss;
    ss << file.rdbuf();
    // NRVO
    return ss.str();
  }

  throw std::runtime_error{
    format_error_message(program_name,
                         format("%s: Could not open file", path.string()))};
}

[[nodiscard]] program_options::options_description create_options_description()
{
  program_options::options_description desc{"Options"};

  // clang-format off
  desc.add_options()
    ("help,h", "Display this information.")
    ("version,v", "Display compiler version.")
    ("input", program_options::value<std::string>(), "Input instead of files.")
    ("llvmir", "Output LLVM IR instead of Object code.")
    ("jit", "Perform jit compilation.")
    ("opt", program_options::value<bool>()->default_value(true),
      "With or without optimization.")
    ("relocation-model",
      program_options::value<std::string>()->default_value("pic"),
      "Set the relocation model. Possible values are 'static' or 'pic'.")
    ("input-file", program_options::value<std::vector<std::string>>(), "Input file.")
    ;
  // clang-format on

  return desc;
}

[[nodiscard]] program_options::variables_map
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

[[nodiscard]] std::vector<std::string>
get_input_files(const std::string_view                program_name,
                const program_options::variables_map& vm)
{
  if (vm.contains("input-file"))
    return vm["input-file"].as<std::vector<std::string>>();
  else {
    throw std::runtime_error{
      maple::format_error_message(program_name, "no input files", true)};
  }
}

[[nodiscard]] std::string string_to_lower(const std::string& str)
{
  std::string result;

  // Relocation model string to lower.
  std::transform(str.begin(),
                 str.end(),
                 std::back_inserter(result),
                 [](auto&& ch) { return std::tolower(ch); });

  return result;
}

[[nodiscard]] llvm::Reloc::Model
get_relocation_model(const std::string_view                program_name,
                     const program_options::variables_map& vm)
{
  const auto& rm_str = vm["relocation-model"].as<std::string>();

  const auto rm_lower_str = string_to_lower(rm_str);

  if (rm_lower_str == "static")
    return llvm::Reloc::Model::Static;
  else if (rm_lower_str == "pic")
    return llvm::Reloc::Model::PIC_;
  else {
    throw std::runtime_error{maple::format_error_message(
      program_name,
      format("The value '%s' for --relocation-model is invalid!", rm_lower_str),
      true)};
  }
}

void display_version()
{
  const auto major = MAPLE_VER / 100000;
  const auto minor = MAPLE_VER / 100 % 1000;
  const auto patch = MAPLE_VER % 100;

  std::cout << major << '.' << minor << '.' << patch << '\n';
}

[[noreturn]] void unreachable_internal(const std::size_t line, const char* file)
{
#ifndef NDEBUG
  if (file) {
    std::cerr << "Unreachable executed"
              << " at " << file << ":" << line << '!' << std::endl;
  }
#endif

#if defined(__GNUC__) // GCC, Clang, ICC
  __builtin_unreachable();
#elif define(_MSC_VER) // MSVC
  __assume(false);
#endif
}

} // namespace maple
