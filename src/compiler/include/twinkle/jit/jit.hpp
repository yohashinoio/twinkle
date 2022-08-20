/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _3c683dd0_a929_11ec_b909_0242ac120002
#define _3c683dd0_a929_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>

namespace twinkle::jit
{

struct JitCompiler {
  JitCompiler(std::unique_ptr<llvm::orc::ExecutionSession> exec_session,
              llvm::orc::JITTargetMachineBuilder           jit_tmb,
              llvm::DataLayout                             data_layout);

  ~JitCompiler();

  [[nodiscard]] static llvm::Expected<std::unique_ptr<JitCompiler>> create();

  [[nodiscard]] const llvm::DataLayout& getDataLayout() const;

  [[nodiscard]] llvm::orc::JITDylib& getMainJitDylib();

  [[nodiscard]] llvm::Error
  addModule(llvm::orc::ThreadSafeModule  thread_safe_module,
            llvm::orc::ResourceTrackerSP resource_tracker = nullptr);

  [[nodiscard]] llvm::Expected<llvm::JITEvaluatedSymbol>
  lookup(const llvm::StringRef name);

private:
  std::unique_ptr<llvm::orc::ExecutionSession> exec_session;

  llvm::DataLayout             data_layout;
  llvm::orc::MangleAndInterner mangle;

  llvm::orc::RTDyldObjectLinkingLayer object_layer;
  llvm::orc::IRCompileLayer           compile_layer;

  llvm::orc::JITDylib& main_jd;
};

} // namespace twinkle::jit

#endif
