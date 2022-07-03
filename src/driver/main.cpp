/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <emera/compile/main.hpp>
#include <cstdlib>

int main(const int argc, const char* const* const argv)
{
  const auto compile_res = emera::compile::main(argc, argv);

  if (!compile_res.success())
    return EXIT_FAILURE;

  if (const auto& retval_from_main = compile_res.getJitResult())
    return *retval_from_main; // Return value from main.

  // TODO: link

  return EXIT_SUCCESS;
}
