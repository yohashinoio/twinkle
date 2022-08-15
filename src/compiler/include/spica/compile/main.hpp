/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _a97155b6_b3dc_11ec_b909_0242ac120002
#define _a97155b6_b3dc_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <context.hpp>
#include <optional>
#include <variant>

namespace spica
{

struct JITResult {
  explicit JITResult(const int exit_status) noexcept
    : exit_status{exit_status}
  {
  }

  const int exit_status;
};

struct AOTResult {};

using CompileResult = std::variant<JITResult, AOTResult>;

std::optional<CompileResult> compile(const Context&         ctx,
                                     const std::string_view argv_front);

} // namespace spica

#endif
