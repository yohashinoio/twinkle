/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <emera/compile/main.hpp>
#include <emera/codegen/codegen.hpp>
#include <emera/jit/jit.hpp>
#include <emera/parse/parser.hpp>
#include <emera/option/option.hpp>
#include <emera/support/file.hpp>
#include <emera/support/utils.hpp>
#include <emera/support/exception.hpp>

namespace program_options = boost::program_options;

namespace emera::compile
{

static bool isBackNewline(const char* str) noexcept
{
  for (;;) {
    if (*str == '\0')
      return *--str == '\n';
    ++str;
  }

  unreachable();
}

static std::ostream& printHelp(std::ostream&          ostm,
                               const std::string_view command,
                               const program_options::options_description& desc)
{
  fmt::print(ostm, "Usage: {} [options] file...\n", command);
  return ostm << desc;
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
    std::cout << "emera version " << getVersion() << std::endl;
    std::exit(EXIT_SUCCESS);
  }
  else if (vmap.contains("help")) {
    printHelp(std::cout, *argv, desc);
    std::exit(EXIT_SUCCESS);
  }

  std::vector<parse::Parser::Result> parse_results;

  for (const auto& path : getInputFiles(*argv, vmap)) {
    parse_results.emplace_back(
      parse::Parser{loadFile(*argv, path), path}.getResult());
  }

  codegen::CodeGenerator code_generator{*argv,
                                        std::move(parse_results),
                                        vmap["Opt"].as<unsigned int>(),
                                        getRelocationModel(*argv, vmap)};

  if (vmap.contains("JIT")) {
    return {true, code_generator.doJIT()};
  }
  else {
    emitFile(code_generator, vmap);
    return {true, std::nullopt};
  }

  unreachable();
}
catch (const program_options::error& err) {
  // Error about command line options.
  std::cerr << formatError(*argv, err.what())
            << (isBackNewline(err.what()) ? "" : "\n") << std::flush;

  return {false, std::nullopt};
}
catch (const ErrorBase& err) {
  std::cerr << err.what() << (isBackNewline(err.what()) ? "" : "\n")
            << std::flush;

  return {false, std::nullopt};
}

} // namespace emera::compile
