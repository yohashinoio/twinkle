/**
 * main.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <compile/main.hpp>
#include <codegen/codegen.hpp>
#include <jit/jit.hpp>
#include <parse/parse.hpp>
#include <utils/util.hpp>
#include <utils/format.hpp>

namespace program_options = boost::program_options;

static bool is_back_newline(const char* str) noexcept
{
  for (;;) {
    if (*str == '\0')
      return *--str == '\n';
    ++str;
  }
}

static std::ostream&
output_help(std::ostream&                               ostm,
            const std::string_view                      command,
            const program_options::options_description& desc)
{
  return ostm << maple::format("Usage: %s [options] file...\n", command.data())
              << desc;
}

// Emit object file without error even if target does not exist.
static void emit_file(maple::codegen::CodeGenerator&        generator,
                      const std::filesystem::path&          path,
                      const program_options::variables_map& vmap)
{
  const std::string_view target = vmap["emit"].as<std::string>();

  if (target == "llvm")
    generator.emit_llvmIR_file(path.stem().string() + ".ll");
  else if (target == "asm")
    generator.emit_assembly_file(path.stem().string() + ".s");
  else
    generator.emit_object_file(path.stem().string() + ".o");
}

namespace maple::compile
{

CompileResult
main(const int argc, const char* const* const argv, const bool eout)
try {
  const auto desc = create_options_description();

  const auto vmap = get_variable_map(desc, argc, argv);

  if (argc == 1) {
    output_help(std::cerr, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }
  if (vmap.contains("version")) {
    maple::display_version();
    std::exit(EXIT_SUCCESS);
  }
  else if (vmap.contains("help")) {
    output_help(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  const auto opt = vmap["opt"].as<bool>();

  const auto relocation_model = get_relocation_model(*argv, vmap);

  auto file_paths = get_input_files(*argv, vmap);

  for (auto&& file_path : file_paths) {
    auto input = load_file_to_string(*argv, file_path);

    parse::Parser parser{std::move(input), file_path, eout};

    codegen::CodeGenerator generator{*argv,
                                     parser.get_ast(),
                                     parser.get_positions(),
                                     file_path,
                                     opt,
                                     relocation_model};

    if (vmap.contains("jit"))
      return {true, generator.do_JIT()};
    else
      emit_file(generator, file_path, vmap);
  }

  return {true, std::nullopt};
}
catch (const program_options::error& err) {
  if (eout) {
    // Error about command line options.
    std::cerr << format_error_message(*argv, err.what(), true)
              << (is_back_newline(err.what()) ? "" : "\n")
              << "compilation terminated." << std::endl;
  }

  return {false, std::nullopt};
}
catch (const std::runtime_error& err) {
  if (eout) {
    std::cerr << err.what() << (is_back_newline(err.what()) ? "" : "\n")
              << "compilation terminated." << std::endl;
  }

  return {false, std::nullopt};
}

} // namespace maple::compile
