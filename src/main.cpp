//
//  main.cpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#include "parse.hpp"
#include "codegen.hpp"
#include "utility.hpp"

namespace program_options = boost::program_options;

std::ostream& output_help(std::ostream&          ostm,
                          const std::string_view exe_file_name,
                          const program_options::options_description& desc)
{
  return ostm << miko::format("Usage: %s [options] file...\n",
                              exe_file_name.data())
              << desc;
}

bool is_back_newline(const char* str) noexcept
{
  for (;;) {
    if (*str == '\0')
      return *--str == '\n';
    ++str;
  }
}

void output(miko::codegen::code_generator&        generator,
            const program_options::variables_map& vm,
            const std::filesystem::path&          path = "a.o")
{
  if (vm.count("irprint"))
    generator.stdout_llvm_ir();
  if (vm.count("output"))
    generator.write_object_code_to_file(vm["output"].as<std::string>());
  else
    generator.write_object_code_to_file(path);
}

int main(const int argc, const char* const* const argv)
try {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  const auto desc = miko::create_options_description();
  const auto vm   = miko::get_variable_map(desc, argc, argv);

  if (argc == 1) {
    output_help(std::cerr, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }
  else if (vm.count("version")) {
    miko::display_version();
    std::exit(EXIT_SUCCESS);
  }
  else if (vm.count("help")) {
    output_help(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  if (vm.count("input")) {
    std::string_view file_path = "input";

    miko::parse::parser parser{vm["input"].as<std::string>(), file_path};

    miko::codegen::code_generator generator{file_path, parser.parse()};
    generator.codegen();

    output(generator, vm);
  }
  else {
    auto file_paths = miko::get_input_files(vm);

    // TODO: multi thread
    for (auto&& file_path : file_paths) {
      miko::parse::parser parser{miko::load_file_to_string(file_path),
                                 file_path};

      miko::codegen::code_generator generator{file_path, parser.parse()};
      generator.codegen();

      output(generator,
             vm,
             std::filesystem::path{file_path}.stem().string() + ".o");
    }
  }
}
catch (const std::exception& err) {
  std::cerr << err.what() << (is_back_newline(err.what()) ? "" : "\n")
            << "compilation terminated." << std::endl;
  std::exit(EXIT_FAILURE);
}
