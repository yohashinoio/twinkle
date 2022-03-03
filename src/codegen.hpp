//
//  codegen.hpp
//
//  Copyright (c) 2022 The Miko Authors. All rights reserved.
//  MIT License
//

#ifndef _7d00490e_93b3_11ec_b909_0242ac120002
#define _7d00490e_93b3_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "pch.hpp"
#include "ast.hpp"

namespace miko::codegen
{

struct code_generator {
  code_generator(const std::filesystem::path& source, ast::program&& ast);

  auto stdout_llvm_ir() const -> void;

  auto write_object_code_to_file(const std::filesystem::path& out) const
    -> void;

  auto codegen() -> void;

private:
  llvm::LLVMContext             context;
  llvm::IRBuilder<>             builder;
  std::shared_ptr<llvm::Module> module;

  const std::filesystem::path& source;

  ast::program ast;
};

} // namespace miko::codegen

#endif
