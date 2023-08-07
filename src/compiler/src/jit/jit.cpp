/**
 * These codes are licensed under MIT License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/jit/jit.hpp>
#include <twinkle/support/utils.hpp>

namespace twinkle::jit
{

JitCompiler::JitCompiler(
  std::unique_ptr<llvm::orc::ExecutionSession>    exec_session,
  std::unique_ptr<llvm::orc::EPCIndirectionUtils> epciu,
  llvm::orc::JITTargetMachineBuilder              jit_tmb,
  llvm::DataLayout                                data_layout)
  : exec_session{std::move(exec_session)}
  , epciu{std::move(epciu)}
  , data_layout{std::move(data_layout)}
  , mangle{*this->exec_session, this->data_layout}
  , object_layer{*this->exec_session,
                 []() {
                   return std::make_unique<llvm::SectionMemoryManager>();
                 }}
  , compile_layer{*this->exec_session,
                  object_layer,
                  std::make_unique<llvm::orc::ConcurrentIRCompiler>(
                    std::move(jit_tmb))}
  , optimize_layer(*this->exec_session, compile_layer, optimizeModule)
  , cod_layer{*this->exec_session,
              optimize_layer,
              this->epciu->getLazyCallThroughManager(),
              [this] {
                return this->epciu->createIndirectStubsManager();
              }}
  , main_jd{this->exec_session->createBareJITDylib("<main>")}
{
  main_jd.addGenerator(llvm::cantFail(
    llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
      data_layout.getGlobalPrefix())));
}

JitCompiler::~JitCompiler()
{
  if (auto err = exec_session->endSession())
    exec_session->reportError(std::move(err));
  if (auto err = epciu->cleanup())
    exec_session->reportError(std::move(err));
}

[[nodiscard]] llvm::Expected<std::unique_ptr<JitCompiler>> JitCompiler::create()
{
  auto epc = llvm::orc::SelfExecutorProcessControl::Create();
  if (!epc)
    return epc.takeError();

  auto exec_session
    = std::make_unique<llvm::orc::ExecutionSession>(std::move(*epc));

  auto epciu = llvm::orc::EPCIndirectionUtils::Create(
    exec_session->getExecutorProcessControl());
  if (!epciu)
    return epciu.takeError();

  (*epciu)->createLazyCallThroughManager(
    *exec_session,
    llvm::pointerToJITTargetAddress(&handleLazyCallThroughError));

  if (auto err = setUpInProcessLCTMReentryViaEPCIU(**epciu))
    return std::move(err);

  llvm::orc::JITTargetMachineBuilder jtmb(
    exec_session->getExecutorProcessControl().getTargetTriple());

  auto dl = jtmb.getDefaultDataLayoutForTarget();
  if (!dl)
    return dl.takeError();

  return std::make_unique<JitCompiler>(std::move(exec_session),
                                       std::move(*epciu),
                                       std::move(jtmb),
                                       std::move(*dl));
}

[[nodiscard]] llvm::Error
JitCompiler::addModule(llvm::orc::ThreadSafeModule  thread_safe_module,
                       llvm::orc::ResourceTrackerSP resource_tracker)
{
  if (!resource_tracker)
    resource_tracker = main_jd.getDefaultResourceTracker();

  return cod_layer.add(resource_tracker, std::move(thread_safe_module));
}

[[nodiscard]] llvm::Expected<llvm::orc::ThreadSafeModule>
JitCompiler::optimizeModule(llvm::orc::ThreadSafeModule tsm,
                            const llvm::orc::MaterializationResponsibility&)
{
  tsm.withModuleDo([](llvm::Module& m) {
    // Create a function pass manager
    auto fpm = std::make_unique<llvm::legacy::FunctionPassManager>(&m);

    // Add some optimizations
    {
      llvm::PassManagerBuilder builder;

      builder.OptLevel = 2;

      builder.populateFunctionPassManager(*fpm);

      fpm->doInitialization();
    }

    // Run the optimizations over all functions in the module being added to
    // the JIT
    for (auto& f : m)
      fpm->run(f);
  });

  return std::move(tsm);
}

} // namespace twinkle::jit
