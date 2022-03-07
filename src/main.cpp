//
//  main.cpp
//
//  Copyright (c) 2022 The Miko Authors.
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

void output_to_file(miko::codegen::code_generator& generator,
                    const std::filesystem::path&   path,
                    const bool                     output_llvmir,
                    const bool                     mem2reg)
{
  if (mem2reg)
    generator.mem2reg();

  if (output_llvmir) {
    // test.xxx -> test.ll
    generator.write_llvm_ir_to_file(path.stem().string() + ".ll");
  }
  else {
    // test.xxx -> test.o
    generator.write_object_code_to_file(path.stem().string() + ".o");
  }
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
  if (vm.count("version")) {
    miko::display_version();
    std::exit(EXIT_SUCCESS);
  }
  else if (vm.count("help")) {
    output_help(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  const auto mem2reg = vm["mem2reg"].as<bool>();

  if (vm.count("input")) {
    std::string_view file_path = "input";

    const std::string&  input = vm["input"].as<std::string>();
    // Parsing is performed as soon as the constructor is called.
    miko::parse::parser parser{input.cbegin(), input.cend(), file_path};

    // Code generation occurs as soon as the constructor is called.
    miko::codegen::code_generator generator{parser.get_ast(),
                                            parser.get_positions(),
                                            file_path};

    output_to_file(generator, file_path, vm.count("llvmir"), mem2reg);
  }
  else {
    auto file_paths = miko::get_input_files(vm);

    for (auto&& file_path : file_paths) {
      const std::string input = miko::load_file_to_string(file_path);

      // Parsing is performed as soon as the constructor is called.
      miko::parse::parser parser{input.cbegin(), input.cend(), file_path};

      // Code generation occurs as soon as the constructor is called.
      miko::codegen::code_generator generator{parser.get_ast(),
                                              parser.get_positions(),
                                              file_path};

      output_to_file(generator, file_path, vm.count("llvmir"), mem2reg);
    }
  }
}
catch (const std::exception& err) {
  std::cerr << err.what() << (is_back_newline(err.what()) ? "" : "\n")
            << "compilation terminated." << std::endl;
  std::exit(EXIT_FAILURE);
}
