//
//  main.cpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#include "parse.hpp"
#include "codegen.hpp"
#include "utility.hpp"

int main(const int argc, const char* const* const argv)
try {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  namespace program_options = boost::program_options;

  program_options::options_description opt{"Usage"};

  miko::setup_program_options(opt);

  program_options::variables_map opt_map;
  program_options::store(program_options::parse_command_line(argc, argv, opt),
                         opt_map);

  if (argc == 1) {
    // Display usage instructions
    std::cerr << opt;
    std::exit(EXIT_SUCCESS);
  }
  else if (opt_map.count("version")) {
    miko::display_version();
    std::exit(EXIT_SUCCESS);
  }
  else if (opt_map.count("help")) {
    // Display usage.
    std::cout << opt;
    std::exit(EXIT_SUCCESS);
  }

  // const std::filesystem::path path = argv[1];
  const std::filesystem::path source_file_path = "top";

  // auto source = miko::load_file_to_string(source_file_path);
  auto source = argv[argc - 1];

  miko::parse::parser parser{std::move(source), source_file_path};

  miko::codegen::code_generator generator{source_file_path, parser.parse()};
  generator.codegen();

  if (opt_map.count("irprint"))
    generator.llvm_ir_print();

  if (opt_map.count("out")) {
    generator.write_to_file(opt_map["out"].as<std::string>());
  }
  else
    generator.write_to_file("a.o");
}
catch (const std::exception& err) {
  std::cerr << err.what() << std::endl;
  std::exit(EXIT_FAILURE);
}
