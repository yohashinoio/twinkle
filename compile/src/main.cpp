/**
 * main.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <compile.hpp>
#include <codegen/codegen.hpp>
#include <jit/jit.hpp>
#include <parse/parse.hpp>
#include <utils/util.hpp>
#include <utils/format.hpp>

namespace program_options = boost::program_options;

namespace miko::compile
{

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

void output_to_file(miko::codegen::CodeGenerator& generator,
                    const std::filesystem::path&  path,
                    const bool                    output_llvmir)
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

CompileResult
main(const int argc, const char* const* const argv, const bool error_output)
try {
  const auto desc  = miko::create_options_description();
  const auto v_map = miko::get_variable_map(desc, argc, argv);

  if (argc == 1) {
    output_help(std::cerr, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }
  if (v_map.count("version")) {
    miko::display_version();
    std::exit(EXIT_SUCCESS);
  }
  else if (v_map.count("help")) {
    output_help(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  const auto optimize = v_map["opt"].as<bool>();

  if (v_map.count("input")) {
    std::string_view file_path = "a";

    miko::parse::Parser parser{v_map["input"].as<std::string>(),
                               file_path,
                               error_output};

    miko::codegen::CodeGenerator generator{*argv,
                                           parser.get_ast(),
                                           parser.get_positions(),
                                           file_path,
                                           optimize};

    if (v_map.count("jit"))
      return {true, generator.jit_compile()};
    else
      output_to_file(generator, file_path, v_map.count("llvmir"));
  }
  else {
    auto file_paths = miko::get_input_files(*argv, v_map);

    for (auto&& file_path : file_paths) {
      auto input = miko::load_file_to_string(*argv, file_path);

      miko::parse::Parser parser{std::move(input), file_path, error_output};

      miko::codegen::CodeGenerator generator{*argv,
                                             parser.get_ast(),
                                             parser.get_positions(),
                                             file_path,
                                             optimize};

      if (v_map.count("jit"))
        return {true, generator.jit_compile()};
      else
        output_to_file(generator, file_path, v_map.count("llvmir"));
    }
  }

  return {true, std::nullopt};
}
catch (const program_options::error& err) {
  if (error_output) {
    // Error about command line options.
    std::cerr << miko::format_error_message(*argv, err.what(), true)
              << (is_back_newline(err.what()) ? "" : "\n")
              << "compilation terminated." << std::endl;
  }

  return {false, std::nullopt};
}
catch (const std::runtime_error& err) {
  if (error_output) {
    std::cerr << err.what() << (is_back_newline(err.what()) ? "" : "\n")
              << "compilation terminated." << std::endl;
  }

  return {false, std::nullopt};
}

} // namespace miko::compile
