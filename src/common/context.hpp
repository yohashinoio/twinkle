/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _dc157f0a_3848_4f5c_80df_e89dd33c2b21
#define _dc157f0a_3848_4f5c_80df_e89dd33c2b21

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>
#include <vector>
#include <optional>

namespace spica
{

constexpr unsigned int DEFAULT_OPT_LEVEL = 2;

#define EMIT_EXE_ARG    "exe"
#define EMIT_OBJ_ARG    "obj"
#define EMIT_ASM_ARG    "asm"
#define EMIT_LLVMIR_ARG "llvm"

struct Context {
  Context(std::vector<std::string>&& input_files,
          const bool                 jit,
          std::string&&              emit_target,
          const unsigned int         opt_level,
          std::string&&              relocation_model,
          std::vector<std::string>&& linked_libs) noexcept
    : input_files{std::move(input_files)}
    , jit{jit}
    , emit_target{std::move(emit_target)}
    , opt_level{opt_level}
    , relocation_model{std::move(relocation_model)}
    , linked_libs{std::move(linked_libs)}
  {
  }

  const std::vector<std::string> input_files;

  const bool jit;

  const std::string emit_target;

  const unsigned int opt_level;

  const std::string relocation_model;

  const std::vector<std::string> linked_libs;
};

} // namespace spica

#endif
