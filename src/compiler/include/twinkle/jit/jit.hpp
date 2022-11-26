/**
 * These codes are licensed under LGPL-2.1 License
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
  JitCompiler(std::unique_ptr<llvm::orc::ExecutionSession>    exec_session,
              std::unique_ptr<llvm::orc::EPCIndirectionUtils> epciu,
              llvm::orc::JITTargetMachineBuilder              jit_tmb,
              llvm::DataLayout                                data_layout);

  ~JitCompiler();

  [[nodiscard]] static llvm::Expected<std::unique_ptr<JitCompiler>> create();

  [[nodiscard]] const llvm::DataLayout& getDataLayout() const
  {
    return data_layout;
  }

  [[nodiscard]] llvm::orc::JITDylib& getMainJitDylib()
  {
    return main_jd;
  }

  [[nodiscard]] llvm::Error
  addModule(llvm::orc::ThreadSafeModule  thread_safe_module,
            llvm::orc::ResourceTrackerSP resource_tracker = nullptr);

  [[nodiscard]] llvm::Expected<llvm::JITEvaluatedSymbol>
  lookup(const llvm::StringRef name)
  {
    return exec_session->lookup({&main_jd}, mangle(name.str()));
  }

private:
  std::unique_ptr<llvm::orc::ExecutionSession>    exec_session;
  std::unique_ptr<llvm::orc::EPCIndirectionUtils> epciu;

  llvm::DataLayout             data_layout;
  llvm::orc::MangleAndInterner mangle;

  llvm::orc::RTDyldObjectLinkingLayer object_layer;
  llvm::orc::IRCompileLayer           compile_layer;
  llvm::orc::IRTransformLayer         optimize_layer;
  llvm::orc::CompileOnDemandLayer     cod_layer;

  llvm::orc::JITDylib& main_jd;

  static void handleLazyCallThroughError()
  {
    llvm::errs() << "LazyCallThrough error: Could not find function body";
    exit(1);
  }

  static llvm::Expected<llvm::orc::ThreadSafeModule>
  optimizeModule(llvm::orc::ThreadSafeModule tsm,
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
};

} // namespace twinkle::jit

#endif
