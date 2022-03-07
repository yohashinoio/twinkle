//
//  codegen.hpp
//
//  Copyright (c) 2022 The Miko Authors.
//  MIT License
//

#ifndef _7d00490e_93b3_11ec_b909_0242ac120002
#define _7d00490e_93b3_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "pch.hpp"
#include "ast.hpp"
#include "utility.hpp"

namespace miko::codegen
{

struct code_generator {
  code_generator(const ast::program&          ast,
                 const position_cache&        positions,
                 const std::filesystem::path& source,
                 const bool                   optimize);

  void write_llvm_ir_to_file(const std::filesystem::path& out) const;

  void write_object_code_to_file(const std::filesystem::path& out) const;

  void codegen();

private:
  llvm::LLVMContext             context;
  std::shared_ptr<llvm::Module> module;
  llvm::IRBuilder<>             builder;

  llvm::legacy::FunctionPassManager fpm;

  const std::filesystem::path& source;

  const ast::program&   ast;
  const position_cache& positions;
};

} // namespace miko::codegen

#endif
