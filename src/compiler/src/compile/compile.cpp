/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/compile/compile.hpp>
#include <twinkle/codegen/codegen.hpp>
#include <twinkle/jit/jit.hpp>
#include <twinkle/parse/parser.hpp>
#include <twinkle/support/file.hpp>
#include <twinkle/support/utils.hpp>
#include <twinkle/support/exception.hpp>

namespace twinkle
{

// Emit object file without error even if target does not exist
// Returns the created file paths
static FilePaths emitFile(codegen::CodeGenerator& generator,
                          const std::string&      target)
{
  if (target == EMIT_EXE_ARG)
    return generator.emitTemporaryObjectFiles();

  if (target == EMIT_OBJ_ARG)
    return generator.emitObjectFiles();

  if (target == EMIT_ASM_ARG)
    return generator.emitAssemblyFiles();

  if (target == EMIT_LLVMIR_ARG)
    return generator.emitLlvmIRFiles();

  unreachable();
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
    getRelocationModel(ctx.relocation_model, argv_front),
    ctx.target_triple,
    ctx.jit};

  if (ctx.jit)
    return JITResult{code_generator.doJIT()};
  else
    return AOTResult{emitFile(code_generator, ctx.emit_target)};

  unreachable();
}
catch (const ErrorBase& err) {
  std::cerr << err.what() << (isBackNewline(err.what()) ? "" : "\n")
            << std::flush;

  return std::nullopt;
}

} // namespace twinkle
