/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <spica/compile/main.hpp>
#include <spica/codegen/codegen.hpp>
#include <spica/jit/jit.hpp>
#include <spica/parse/parser.hpp>
#include <spica/support/file.hpp>
#include <spica/support/utils.hpp>
#include <spica/support/exception.hpp>

namespace spica
{

// Emit object file without error even if target does not exist
static void emitFile(codegen::CodeGenerator&           generator,
                     const std::optional<std::string>& emit_target)
{
  if (emit_target) {
    const auto target = *emit_target;

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

[[nodiscard]] static llvm::Reloc::Model
getRelocationModel(const std::string_view reloc_model,
                   const std::string_view argv_front)
{
  if (reloc_model == "static")
    return llvm::Reloc::Model::Static;
  else if (reloc_model == "pic")
    return llvm::Reloc::Model::PIC_;
  else {
    throw ErrorBase{formatError(
      argv_front,
      fmt::format("the value '{}' for --relocation-model is invalid!",
                  reloc_model))};
  }
}

std::optional<CompileResult> compile(const Context&         ctx,
                                     const std::string_view argv_front)
try {
  std::vector<parse::Parser::Result> parse_results;

  for (const auto& path : ctx.input_files) {
    parse_results.emplace_back(
      parse::Parser{loadFile(argv_front, path), path}.getResult());
  }

  codegen::CodeGenerator code_generator{
    argv_front,
    std::move(parse_results),
    ctx.opt_level,
    getRelocationModel(ctx.relocation_model, argv_front)};

  if (ctx.jit)
    return JITResult{code_generator.doJIT()};
  else {
    emitFile(code_generator, ctx.emit_target);
    return AOTResult{};
  }

  unreachable();
}
catch (const ErrorBase& err) {
  std::cerr << err.what() << (isBackNewline(err.what()) ? "" : "\n")
            << std::flush;

  return std::nullopt;
}

} // namespace spica
