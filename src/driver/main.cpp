/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <compile/main.hpp>
#include <cstdlib>

int main(const int argc, const char* const* const argv)
{
  const auto c_result = custard::compile::main(argc, argv);

  if (!c_result.success) {
    // Failure.
    return EXIT_FAILURE;
  }

  if (c_result.jit_result) {
    // JIT compiled.
    return *c_result.jit_result; // Return value from main.
  }

  // TODO: link

  return EXIT_SUCCESS;
}
