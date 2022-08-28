/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/codegen/codegen.hpp>
#include <twinkle/codegen/top_level.hpp>
#include <twinkle/codegen/type.hpp>
#include <twinkle/codegen/exception.hpp>
#include <twinkle/unicode/unicode.hpp>
#include <cassert>
#include <boost/filesystem.hpp>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

namespace
{

[[nodiscard]] std::string createTemporaryFilepath()
{
  return (boost::filesystem::temp_directory_path()
          / boost::filesystem::unique_path())
    .native();
}

} // namespace

namespace twinkle::codegen
{

[[nodiscard]] std::optional<std::shared_ptr<Type>>
CreatedClassTemplateTable::operator[](
  const CreatedClassTemplateTableKey& key) const
{
  for (const auto& r : table) {
    auto& [name, args, namespace_] = r.first;

    const auto template_arg_types_of_keyarg = std::get<1>(key);

    if (std::get<0>(key) == name) {
      for (std::size_t idx = 0;
           const auto& type : template_arg_types_of_keyarg.types) {
        const auto pos
          = ctx.positions.position_of(template_arg_types_of_keyarg);

        const auto t1 = createType(ctx, args.types.at(idx), pos);
        const auto t2 = createType(ctx, type, pos);

        assert(t1->getLLVMType(ctx) && t2->getLLVMType(ctx));

        // Mangled name comparisons allow pure type comparisons (aliased types
        // can also be compared to ordinary types)
        if (t1->getMangledName(ctx) == t2->getMangledName(ctx))
          return r.second;

        ++idx;
      }
    }
  }

  return std::nullopt;
}

void CreatedClassTemplateTable::insert(CreatedClassTemplateTableKey&& key,
                                       const std::shared_ptr<Type>&   value)
{
  const auto r = table.insert(
    std::move(CreatedClassTemplateTableElem{std::move(key), value}));

  assert(r.second);
}

template <typename R = std::vector<std::string>>
[[nodiscard]] static R splitByLine(const std::string& str)
{
  std::istringstream ss{str};
  R                  lines;

  for (;;) {
    std::string line;
    if (std::getline(ss, line))
      lines.emplace_back(line);
    else
      return lines;
  }

  unreachable();
}

//===----------------------------------------------------------------------===//
// Code generator
//===----------------------------------------------------------------------===//

CGContext::CGContext(llvm::LLVMContext&      context,
                     PositionCache&&         positions,
                     std::filesystem::path&& file,
                     const std::string&      source_code,
                     const unsigned int      opt_level) noexcept
  : context{context}
  , module{std::make_unique<llvm::Module>(file.filename().string(), context)}
  , builder{context}
  , file{std::move(file)}
  , created_class_template_table{*this}
  , positions{std::move(positions)}
  , source_code{splitByLine(source_code)}
  , fpm{module.get()}
{
  // Setup pass manager
  {
    llvm::PassManagerBuilder builder;

    builder.OptLevel = opt_level;

    builder.populateFunctionPassManager(fpm);

    fpm.doInitialization();
  }
}

[[nodiscard]] std::string
CGContext::formatError(const PositionRange&   pos,
                       const std::string_view message) const
{
  const auto rows = calcRows(pos);

  return fmt::format("In file {}, line {}:\n", file.string(), rows)
         + fmt::format(fg(fmt::terminal_color::bright_red), "error: ")
         + fmt::format(fg(fmt::terminal_color::bright_white), "{}\n", message)
         + boost::algorithm::trim_copy(source_code.at(rows - 1));
}

[[nodiscard]] std::size_t CGContext::calcRows(const PositionRange& pos) const
{
  std::size_t rows{};

  for (auto iter = pos.begin();; --iter) {
    if (*iter == U'\n')
      ++rows;

    if (iter == positions.first())
      return ++rows;
  }

  unreachable();
}

CodeGenerator::CodeGenerator(const std::string_view               argv_front,
                             std::vector<parse::Parser::Result>&& parse_results,
                             const unsigned int                   opt_level,
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
    verifyOptLevel(opt_level);

    CGContext ctx{*context,
                  std::move(it->positions),
                  std::move(it->file),
                  it->input,
                  opt_level};

    ctx.module->setTargetTriple(target_triple);
    ctx.module->setDataLayout(target_machine->createDataLayout());

    codegen(it->ast, ctx);

    results.emplace_back(std::move(ctx.module), std::move(ctx.file));
  }
}

void CodeGenerator::verifyOptLevel(const unsigned int opt_level) const
{
  switch (opt_level) {
  case 0:
  case 1:
  case 2:
  case 3:
    return;
  default:
    throw CodegenError{formatError(argv_front, "invalid optimization level")};
  }
}

[[nodiscard]] FilePaths CodeGenerator::emitLlvmIRFiles()
{
  FilePaths created_files;

  for (auto it = results.begin(), last = results.end(); it != last; ++it) {
    const auto& file = std::get<std::filesystem::path>(*it);

    const auto output_file = file.stem().string() + ".ll";

    created_files.push_back(output_file);

    std::error_code      ostream_ec;
    llvm::raw_fd_ostream os{output_file,
                            ostream_ec,
                            llvm::sys::fs::OpenFlags::OF_None};

    if (ostream_ec) {
      throw CodegenError{formatError(
        argv_front,
        fmt::format("{}: {}", file.string(), ostream_ec.message()))};
    }

    std::get<std::unique_ptr<llvm::Module>>(*it)->print(os, nullptr);
  }

  return created_files;
}

[[nodiscard]] FilePaths CodeGenerator::emitAssemblyFiles()
{
  return emitFiles(llvm::CGFT_AssemblyFile);
}

[[nodiscard]] FilePaths CodeGenerator::emitObjectFiles()
{
  return emitFiles(llvm::CGFT_ObjectFile);
}

[[nodiscard]] FilePaths CodeGenerator::emitTemporaryObjectFiles()
{
  return emitFiles(llvm::CGFT_ObjectFile, true);
}

[[nodiscard]] int CodeGenerator::doJIT()
{
  assert(!jit_compiled);

  jit_compiled = true;

  auto jit_expected = jit::JitCompiler::create();
  if (auto err = jit_expected.takeError())
    throw CodegenError{formatError(argv_front, llvm::toString(std::move(err)))};

  auto jit = std::move(*jit_expected);

  auto [front_module, file] = std::move(results.front());

  // Link all modules
  for (auto it = results.begin() + 1, last = results.end(); it != last; ++it) {
    auto [module, file] = std::move(*it);

    if (llvm::Linker::linkModules(*front_module, std::move(module))) {
      throw CodegenError{
        formatError(argv_front,
                    fmt::format("{}: Could not link", file.string()))};
    }
  }

  if (auto err
      = jit->addModule({std::move(front_module), std::move(context)})) {
    throw CodegenError{
      formatError(file.string(), llvm::toString(std::move(err)))};
  }

  auto symbol_expected = jit->lookup("main");
  if (auto err = symbol_expected.takeError()) {
    throw CodegenError{
      formatError(argv_front, "symbol main could not be found")};
  }

  auto       symbol = *symbol_expected;
  auto const main_addr
    = reinterpret_cast<int (*)(/* TODO: command line arguments */)>(
      symbol.getAddress());

  // Run main
  return main_addr();
}

void CodeGenerator::codegen(const ast::TranslationUnit& ast, CGContext& ctx)
{
  for (const auto& node : ast)
    createTopLevel(ctx, node);

  {
    // Verify module
    std::string              str;
    llvm::raw_string_ostream stream{str};
    if (llvm::verifyModule(*ctx.module, &stream))
      throw CodegenError{formatError(argv_front, stream.str())};
  }
}

[[nodiscard]] FilePaths
CodeGenerator::emitFiles(const llvm::CodeGenFileType cgft,
                         const bool                  create_as_tmpfile)
{
  static const std::unordered_map<llvm::CodeGenFileType, std::string>
    extension_map = {
      {llvm::CodeGenFileType::CGFT_AssemblyFile, "s"},
      {  llvm::CodeGenFileType::CGFT_ObjectFile, "o"}
  };

  FilePaths created_files;

  for (auto it = results.begin(), last = results.end(); it != last; ++it) {
    const auto& file = std::get<std::filesystem::path>(*it);

    const auto output_file
      = (create_as_tmpfile ? createTemporaryFilepath() : file.stem().string())
        + "." + extension_map.at(cgft);

    created_files.push_back(output_file);

    std::error_code      ostream_ec;
    llvm::raw_fd_ostream ostream{output_file,
                                 ostream_ec,
                                 llvm::sys::fs::OpenFlags::OF_None};

    if (ostream_ec) {
      throw CodegenError{formatError(
        argv_front,
        fmt::format("{}: {}\n", file.string(), ostream_ec.message()))};
    }

    llvm::legacy::PassManager p_manager;

    if (target_machine->addPassesToEmitFile(p_manager,
                                            ostream,
                                            nullptr,
                                            cgft)) {
      throw CodegenError{formatError(argv_front, "failed to emit a file")};
    }

    p_manager.run(*std::get<std::unique_ptr<llvm::Module>>(*it));
    ostream.flush();
  }

  return created_files;
}

void CodeGenerator::initTargetTripleAndMachine()
{
  // Set target triple and data layout to module.
  target_triple = llvm::sys::getDefaultTargetTriple();

  std::string target_triple_error;
  auto const  target
    = llvm::TargetRegistry::lookupTarget(target_triple, target_triple_error);

  if (!target) {
    throw CodegenError{formatError(argv_front,
                                   fmt::format("failed to lookup target {}: {}",
                                               target_triple,
                                               target_triple_error))};
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

} // namespace twinkle::codegen
