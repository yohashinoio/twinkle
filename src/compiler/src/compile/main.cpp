/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <compile/main.hpp>
#include <codegen/codegen.hpp>
#include <jit/jit.hpp>
#include <parse/parser.hpp>
#include <option/parse.hpp>
#include <support/file.hpp>
#include <support/utils.hpp>
#include <support/format.hpp>
#include <support/exception.hpp>

namespace program_options = boost::program_options;

namespace maple::compile
{

static bool isBackNewline(const char* str) noexcept
{
  for (;;) {
    if (*str == '\0')
      return *--str == '\n';
    ++str;
  }
}

static std::ostream& printHelp(std::ostream&          ostm,
                               const std::string_view command,
                               const program_options::options_description& desc)
{
  return ostm << format("Usage: %s [options] file...\n", command.data())
              << desc;
}

// Emit object file without error even if target does not exist.
static void emitFile(codegen::CodeGenerator&               generator,
                     const program_options::variables_map& vmap)
{
  if (vmap.contains("emit")) {
    const auto target = stringToLower(vmap["emit"].as<std::string>());

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

CompileResult main(const int argc, const char* const* const argv)
try {
  const auto desc = createOptionsDesc();

  const auto vmap = getVariableMap(desc, argc, argv);

  if (argc == 1) {
    printHelp(std::cerr, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }
  if (vmap.contains("version")) {
    std::cout << getVersion() << std::endl;
    std::exit(EXIT_SUCCESS);
  }
  else if (vmap.contains("help")) {
    printHelp(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  std::vector<parse::Parser::Result> asts;

  for (const auto& file_path : getInputFiles(*argv, vmap)) {
    auto input = loadFile(*argv, file_path);

    parse::Parser parser{std::move(input), std::move(file_path)};

    asts.push_back(parser.getResult());
  }

  codegen::CodeGenerator generator{*argv,
                                   std::move(asts),
                                   vmap["opt"].as<bool>(),
                                   getRelocationModel(*argv, vmap)};

  if (vmap.contains("JIT")) {
    return {true, generator.doJIT()};
  }
  else {
    emitFile(generator, vmap);
    return {true, std::nullopt};
  }

  unreachable();
}
catch (const program_options::error& err) {
  // Error about command line options.
  std::cerr << formatErrorMessage(*argv, err.what(), true)
            << (isBackNewline(err.what()) ? "" : "\n") << std::flush;

  return {false, std::nullopt};
}
catch (const ErrorBase& err) {
  std::cerr << err.what() << (isBackNewline(err.what()) ? "" : "\n")
            << std::flush;

  return {false, std::nullopt};
}

} // namespace maple::compile
