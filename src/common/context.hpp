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

struct Context {
  Context(std::vector<std::string>&&   input_files,
          const bool                   jit,
          std::optional<std::string>&& emit_target,
          const unsigned int           opt_level,
          std::string&&                relocation_model) noexcept
    : input_files{std::move(input_files)}
    , jit{jit}
    , emit_target{std::move(emit_target)}
    , opt_level{opt_level}
    , relocation_model{std::move(relocation_model)}
  {
  }

  const std::vector<std::string> input_files;

  const bool jit;

  const std::optional<std::string> emit_target;

  const unsigned int opt_level;

  const std::string relocation_model;
};

} // namespace spica

#endif
