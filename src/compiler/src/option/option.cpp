/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <spica/option/option.hpp>
#include <spica/option/exception.hpp>
#include <spica/support/utils.hpp>

namespace spica
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
    ("Opt,O", program_options::value<unsigned int>()->default_value(2),
      "Specify the optimization level.\n"
      "Possible values are 0 1 2 3 and the meaning is the same as clang.")
    ("relocation-model",
      program_options::value<std::string>()->default_value("pic"),
      "Set the relocation model. Possible values are 'static' or 'pic'.\n"
      "If llvm is specified for the emit option, this option is disabled.")
    ("input-file", program_options::value<std::vector<std::string>>(),
      "Input file. Non-optional arguments are equivalent to this.")
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
    throw OptionError{formatError(program_name, "no input files")};
}

[[nodiscard]] llvm::Reloc::Model
getRelocationModel(const std::string_view                program_name,
                   const program_options::variables_map& vmap)
{
  const auto& reloc_model       = vmap["relocation-model"].as<std::string>();
  const auto  lower_reloc_model = stringToLower(reloc_model);

  if (lower_reloc_model == "static")
    return llvm::Reloc::Model::Static;
  else if (lower_reloc_model == "pic")
    return llvm::Reloc::Model::PIC_;
  else {
    throw OptionError{formatError(
      program_name,
      fmt::format("the value '{}' for --relocation-model is invalid!",
                  reloc_model))};
  }
}

} // namespace spica
