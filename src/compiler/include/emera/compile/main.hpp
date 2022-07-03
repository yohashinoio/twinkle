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

#include <optional>

namespace emera::compile
{

struct CompileResult {
  CompileResult(const bool success_, std::optional<int>&& jit_result)
    : success_{success_}
    , jit_result{std::move(jit_result)}
  {
  }

  bool success() const noexcept
  {
    return success_;
  }

  const std::optional<int>& getJitResult() const noexcept
  {
    return jit_result;
  }

private:
  bool success_;

  // If not JIT compiled, std::nullopt is stored.
  std::optional<int> jit_result;
};

// Compilation.
CompileResult main(const int argc, const char* const* const argv);

} // namespace emera::compile

#endif
