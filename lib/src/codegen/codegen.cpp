/**
 * codegen.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/codegen.hpp>
#include <codegen/top_level.hpp>
#include <utils/type.hpp>
#include <utils/format.hpp>
#include <cassert>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace maple::codegen
{

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

CodeGenerator::Context::Context(llvm::LLVMContext&      context,
                                PositionCache&&         positions,
                                std::filesystem::path&& file) noexcept
  : context{context}
  , module{std::make_unique<llvm::Module>(file.filename().string(), context)}
  , builder{context}
  , file{std::move(file)}
  , positions{std::move(positions)}
{
}

[[nodiscard]] llvm::Value*
CodeGenerator::Context::int1ToBool(llvm::Value* value)
{
  const BuiltinType as{BuiltinTypeKind::bool_};

  return llvm::CastInst::CreateIntegerCast(value,
                                           as.getType(context),
                                           as.isSigned(),
                                           "",
                                           builder.GetInsertBlock());
}

[[nodiscard]] std::string CodeGenerator::Context::formatError(
  const boost::iterator_range<InputIterator> pos,
  const std::string_view                     message,
  const bool                                 with_code)
{
  // Calculate line numbers.
  std::size_t rows = 0;
  for (auto iter = pos.begin();; --iter) {
    if (*iter == '\n')
      ++rows;

    if (iter == positions.first()) {
      ++rows;
      break;
    }
  }

  std::ostringstream ss;

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    ss << "In file " << file.string() << ", line " << rows << ":\n"
       << COLOR_RED "error: " COLOR_DEFAULT << message << '\n';
  }
#else
  ss << "In file " << file.string() << ", line " << line << ":\n"
     << "error: " << message << '\n';
#endif

  if (with_code) {
    std::for_each(pos.begin(), pos.end(), [&](auto&& ch) { ss << ch; });

    // TODO:
    // ss << "\n^_";
  }

  return ss.str();
}

CodeGenerator::CodeGenerator(const std::string_view               argv_front,
                             std::vector<parse::Parser::Result>&& parse_results,
                             const bool                           opt,
                             const llvm::Reloc::Model relocation_model)
  : argv_front{argv_front}
  , context{std::make_unique<llvm::LLVMContext>()}
  , relocation_model{relocation_model}
  , parse_results{parse_results}
{
  results.reserve(parse_results.size());

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  initTargetTripleAndMachine();

  for (auto it = parse_results.begin(), last = parse_results.end(); it != last;
       ++it) {
    Context ctx{*context, std::move(it->positions), std::move(it->file)};

    llvm::legacy::FunctionPassManager fp_manager{ctx.module.get()};

    if (opt) {
      fp_manager.add(llvm::createInstructionCombiningPass());
      fp_manager.add(llvm::createReassociatePass());
      fp_manager.add(llvm::createGVNPass());
      fp_manager.add(llvm::createCFGSimplificationPass());
      fp_manager.add(llvm::createPromoteMemoryToRegisterPass());
      fp_manager.add(llvm::createInstructionCombiningPass());
      fp_manager.add(llvm::createReassociatePass());
    }

    fp_manager.doInitialization();

    ctx.module->setTargetTriple(target_triple);
    ctx.module->setDataLayout(target_machine->createDataLayout());

    codegen(it->ast, ctx, fp_manager);

    results.push_back({std::move(ctx.module), std::move(ctx.file)});
  }
}

void CodeGenerator::emitLlvmIRFiles()
{
  for (auto it = results.begin(), last = results.end(); it != last; ++it) {
    const auto& file = std::get<std::filesystem::path>(*it);

    std::error_code      ostream_ec;
    llvm::raw_fd_ostream os{file.stem().string() + ".ll",
                            ostream_ec,
                            llvm::sys::fs::OpenFlags::OF_None};

    if (ostream_ec) {
      throw std::runtime_error{format_error_message(
        argv_front,
        format("%s: %s", file.string(), ostream_ec.message()))};
    }

    std::get<std::unique_ptr<llvm::Module>>(*it)->print(os, nullptr);
  }
}

void CodeGenerator::emitAssemblyFiles()
{
  emitFiles(llvm::CGFT_AssemblyFile);
}

void CodeGenerator::emitObjectFiles()
{
  emitFiles(llvm::CGFT_ObjectFile);
}

[[nodiscard]] int CodeGenerator::doJIT()
{
  assert(!jit_compiled);

  jit_compiled = true;

  auto jit_expected = jit::JitCompiler::create();
  if (auto err = jit_expected.takeError()) {
    throw std::runtime_error{
      format_error_message(argv_front, llvm::toString(std::move(err)), true)};
  }

  auto jit = std::move(*jit_expected);

  auto [front_module, file] = std::move(results.front());

  // Link all modules.
  for (auto it = results.begin() + 1, last = results.end(); it != last; ++it) {
    auto [module, file] = std::move(*it);

    if (llvm::Linker::linkModules(*front_module, std::move(module))) {
      throw std::runtime_error{
        format_error_message(argv_front,
                             format("%s: Could not link", file.string()))};
    }
  }

  if (auto err
      = jit->addModule({std::move(front_module), std::move(context)})) {
    throw std::runtime_error{
      format_error_message(file.string(), llvm::toString(std::move(err)))};
  }

  auto symbol_expected = jit->lookup("main");
  if (auto err = symbol_expected.takeError()) {
    throw std::runtime_error{
      format_error_message(argv_front, "symbol main could not be found")};
  }

  auto       symbol = *symbol_expected;
  auto const main_addr
    = reinterpret_cast<int (*)(/* TODO: command line arguments */)>(
      symbol.getAddress());

  // Run main.
  return main_addr();
}

void CodeGenerator::codegen(const ast::Program&                ast,
                            Context&                           ctx,
                            llvm::legacy::FunctionPassManager& fp_manager)
{
  for (const auto& node : ast)
    boost::apply_visitor(TopLevelVisitor{ctx, fp_manager}, node);
}

void CodeGenerator::emitFiles(const llvm::CodeGenFileType cgft)
{
  static const std::unordered_map<llvm::CodeGenFileType, std::string>
    extension_map = {
      {llvm::CodeGenFileType::CGFT_AssemblyFile, "s"},
      {  llvm::CodeGenFileType::CGFT_ObjectFile, "o"}
  };

  for (auto it = results.begin(), last = results.end(); it != last; ++it) {
    const auto& file = std::get<std::filesystem::path>(*it);

    std::error_code      ostream_ec;
    llvm::raw_fd_ostream ostream{file.stem().string() + "."
                                   + extension_map.at(cgft),
                                 ostream_ec,
                                 llvm::sys::fs::OpenFlags::OF_None};

    if (ostream_ec) {
      throw std::runtime_error{format_error_message(
        argv_front,
        format("%s: %s\n", file.string(), ostream_ec.message()))};
    }

    llvm::legacy::PassManager p_manager;

    if (target_machine->addPassesToEmitFile(p_manager,
                                            ostream,
                                            nullptr,
                                            cgft)) {
      throw std::runtime_error{
        format_error_message(argv_front, "failed to emit a file", true)};
    }

    p_manager.run(*std::get<std::unique_ptr<llvm::Module>>(*it));
    ostream.flush();
  }
}

void CodeGenerator::initTargetTripleAndMachine()
{
  // Set target triple and data layout to module.
  target_triple = llvm::sys::getDefaultTargetTriple();

  std::string target_triple_error;
  auto const  target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_error);

  if (!target) {
    throw std::runtime_error{
      format_error_message(argv_front,
                           format("failed to lookup target %s: %s",
                                  target_triple,
                                  target_triple_error),
                           true)};
  }

  llvm::TargetOptions target_options;

  target_machine
    = target->createTargetMachine(target_triple,
                                  "generic",
                                  "",
                                  target_options,
                                  llvm::Optional<llvm::Reloc::Model>(
                                    relocation_model)); // Set relocation model.
}

} // namespace maple::codegen
