/**
 * option.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <option/parse.hpp>
#include <option/exception.hpp>
#include <support/format.hpp>
#include <support/utils.hpp>

namespace maple
{

namespace program_options = boost::program_options;

[[nodiscard]] program_options::options_description createOptionsDesc()
{
  program_options::options_description desc{"Options"};

  // clang-format off
  desc.add_options()
    ("help,h", "Display this information.")
    ("version,v", "Display version.")
    ("JIT", "Perform Just-in-time(JIT) compilation.\n"
      "If there are multiple input files, they are linked and executed.")
    ("emit", program_options::value<std::string>(),
      "Set a compilation target. Assembly file is 'asm', "
      "object file is 'obj', LLVM IR is 'llvm'.\n"
      "If there are multiple input files, compile each to the target. Not linked.")
    ("opt", program_options::value<bool>()->default_value(true),
      "With or without optimization.")
    ("relocation-model",
      program_options::value<std::string>()->default_value("pic"),
      "Set the relocation model. Possible values are 'static' or 'pic'.\n"
      "If llvm is specified for the emit option, this option is disabled.")
    ("input-file", program_options::value<std::vector<std::string>>(),
      "Input file. Non-optional arguments are equivalent to this")
    ;
  // clang-format on

  return desc;
}

[[nodiscard]] program_options::variables_map
getVariableMap(const program_options::options_description& desc,
               const int                                   argc,
               const char* const* const                    argv)
{
  program_options::positional_options_description p;

  p.add("input-file", -1);

  program_options::variables_map vmap;
  program_options::store(program_options::command_line_parser(argc, argv)
                           .options(desc)
                           .positional(p)
                           .run(),
                         vmap);
  program_options::notify(vmap);

  return vmap;
}

[[nodiscard]] const std::vector<std::string>&
getInputFiles(const std::string_view                program_name,
              const program_options::variables_map& vmap)
{
  if (vmap.contains("input-file"))
    return vmap["input-file"].as<std::vector<std::string>>();
  else
    throw OptionError{formatErrorMessage(program_name, "no input files", true)};
}

[[nodiscard]] llvm::Reloc::Model
getRelocationModel(const std::string_view                program_name,
                   const program_options::variables_map& vmap)
{
  const auto rm_lower_str
    = stringToLower(vmap["relocation-model"].as<std::string>());

  if (rm_lower_str == "static")
    return llvm::Reloc::Model::Static;
  else if (rm_lower_str == "pic")
    return llvm::Reloc::Model::PIC_;
  else {
    throw OptionError{formatErrorMessage(
      program_name,
      format("The value '%s' for --relocation-model is invalid!", rm_lower_str),
      true)};
  }
}

} // namespace maple
