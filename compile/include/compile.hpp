/**
 * compile.hpp
 *
 * These codes are licensed under Apache-2.0 License.
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

namespace miko::compile
{

struct CompileResult {
  bool               success;
  // If not JIT compiled, std::nullopt is stored.
  std::optional<int> jit_result;
};

// Run front-end.
CompileResult main(const int                argc,
                     const char* const* const argv,
                     const bool               error_output = true);

} // namespace miko::compile

#endif
