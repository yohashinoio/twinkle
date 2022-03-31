/**
 * main.cxx
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/codegen.hxx>
#include <jit/jit.hxx>
#include <parse/parse.hxx>
#include <utils/util.hxx>
#include <utils/format.hxx>

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
                    const bool                     output_llvmir)
{
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

  const auto optimize = vm["opt"].as<bool>();

  if (vm.count("input")) {
    std::string_view file_path = "a";

    // Parsing is performed as soon as the constructor is called.
    miko::parse::parser parser{vm["input"].as<std::string>(), file_path};

    // Code generation occurs as soon as the constructor is called.
    miko::codegen::code_generator generator{*argv,
                                            parser.get_ast(),
                                            parser.get_positions(),
                                            file_path,
                                            optimize};

    if (vm.count("jit"))
      return generator.jit_compile();
    else
      output_to_file(generator, file_path, vm.count("llvmir"));
  }
  else {
    auto file_paths = miko::get_input_files(*argv, vm);

    for (auto&& file_path : file_paths) {
      auto input = miko::load_file_to_string(*argv, file_path);

      // Parsing is performed as soon as the constructor is called.
      miko::parse::parser parser{std::move(input), file_path};

      // Code generation occurs as soon as the constructor is called.
      miko::codegen::code_generator generator{*argv,
                                              parser.get_ast(),
                                              parser.get_positions(),
                                              file_path,
                                              optimize};

      if (vm.count("jit"))
        return generator.jit_compile();
      else
        output_to_file(generator, file_path, vm.count("llvmir"));
    }
  }
}
catch (const program_options::error& err) {
  // Error about command line options.
  std::cerr << miko::format_error_message(*argv, err.what(), true)
            << (is_back_newline(err.what()) ? "" : "\n")
            << "compilation terminated." << std::endl;
  std::exit(EXIT_FAILURE);
}
catch (const std::runtime_error& err) {
  // All compilation errors are caught here.
  std::cerr << err.what() << (is_back_newline(err.what()) ? "" : "\n")
            << "compilation terminated." << std::endl;
  std::exit(EXIT_FAILURE);
}