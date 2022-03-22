/**
 * jit.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <jit/jit.hpp>

namespace miko::jit
{

jit_compiler::jit_compiler(
  std::unique_ptr<llvm::orc::ExecutionSession> exec_session,
  llvm::orc::JITTargetMachineBuilder           jit_tmb,
  llvm::DataLayout                             data_layout)
  : exec_session{std::move(exec_session)}
  , data_layout{std::move(data_layout)}
  , mangle{*this->exec_session, this->data_layout}
  , object_layer{*this->exec_session,
                 []() {
                   return std::make_unique<llvm::SectionMemoryManager>();
                 }}
  , compile_layer{*this->exec_session,
                  object_layer,
                  std::make_unique<llvm::orc::ConcurrentIRCompiler>(
                    std::move(std::move(jit_tmb)))}
  , main_jd{this->exec_session->createBareJITDylib("<main>")}
{
  main_jd.addGenerator(llvm::cantFail(
    llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
      data_layout.getGlobalPrefix())));
}

jit_compiler::~jit_compiler()
{
  if (auto&& err = exec_session->endSession())
    exec_session->reportError(std::move(err));
}

llvm::Expected<std::unique_ptr<jit_compiler>> jit_compiler::create()
{
  auto epc = llvm::orc::SelfExecutorProcessControl::Create();
  if (!epc)
    return epc.takeError();

  auto exec_session
    = std::make_unique<llvm::orc::ExecutionSession>(std::move(*epc));

  llvm::orc::JITTargetMachineBuilder jit_tmb(
    exec_session->getExecutorProcessControl().getTargetTriple());

  auto data_layout = jit_tmb.getDefaultDataLayoutForTarget();
  if (!data_layout)
    return data_layout.takeError();

  return std::make_unique<jit_compiler>(std::move(exec_session),
                                        std::move(jit_tmb),
                                        std::move(*data_layout));
}

const llvm::DataLayout& jit_compiler::get_data_layout() const
{
  return data_layout;
}

llvm::orc::JITDylib& jit_compiler::get_main_jit_dylib()
{
  return main_jd;
}

llvm::Error
jit_compiler::add_module(llvm::orc::ThreadSafeModule  thread_safe_module,
                         llvm::orc::ResourceTrackerSP resource_tracker)
{
  if (!resource_tracker)
    resource_tracker = main_jd.getDefaultResourceTracker();
  return compile_layer.add(resource_tracker, std::move(thread_safe_module));
}

llvm::Expected<llvm::JITEvaluatedSymbol>
jit_compiler::lookup(const llvm::StringRef name)
{
  return exec_session->lookup({&main_jd}, mangle(name.str()));
}

} // namespace miko::jit