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
print_help(std::ostream&                               ostm,
           const std::string_view                      command,
           const program_options::options_description& desc)
{
  return ostm << maple::format("Usage: %s [options] file...\n", command.data())
              << desc;
}

// Emit object file without error even if target does not exist.
static void emit_file(maple::codegen::CodeGenerator&        generator,
                      const program_options::variables_map& vmap)
{
  if (vmap.contains("emit")) {
    const auto target = maple::string_to_lower(vmap["emit"].as<std::string>());

    if (target == "llvm") {
      generator.emitLlvmIRFiles();
      return;
    }
    else if (target == "asm") {
      generator.emitAssemblyFiles();
      return;
    }
  }

  generator.emitObjectFiles();
}

namespace maple::compile
{

CompileResult main(const int argc, const char* const* const argv)
try {
  const auto desc = create_options_description();

  const auto vmap = get_variable_map(desc, argc, argv);

  if (argc == 1) {
    print_help(std::cerr, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }
  if (vmap.contains("version")) {
    maple::display_version();
    std::exit(EXIT_SUCCESS);
  }
  else if (vmap.contains("help")) {
    print_help(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  const auto opt = vmap["opt"].as<bool>();

  const auto relocation_model = get_relocation_model(*argv, vmap);

  auto file_paths = get_input_files(*argv, vmap);

  std::vector<parse::Parser::Result> asts;

  for (const auto& file_path : file_paths) {
    auto input = load_file_to_string(*argv, file_path);

    parse::Parser parser{std::move(input), std::move(file_path)};

    asts.push_back(parser.getResult());
  }

  codegen::CodeGenerator generator{*argv,
                                   std::move(asts),
                                   opt,
                                   relocation_model};

  if (vmap.contains("JIT")) {
    return {true, generator.doJIT()};
  }
  else {
    emit_file(generator, vmap);
    return {true, std::nullopt};
  }

  unreachable();
}
catch (const program_options::error& err) {
  // Error about command line options.
  std::cerr << format_error_message(*argv, err.what(), true)
            << (is_back_newline(err.what()) ? "" : "\n") << std::flush;

  return {false, std::nullopt};
}
catch (const std::runtime_error& err) {
  std::cerr << err.what() << (is_back_newline(err.what()) ? "" : "\n")
            << std::flush;

  return {false, std::nullopt};
}

} // namespace maple::compile
