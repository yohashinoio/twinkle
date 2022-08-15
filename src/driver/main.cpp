/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include "cmd.hpp"
#include <spica/compile/main.hpp>
#include <cstdlib>

int main(const int argc, const char* const* const argv)
{
  const auto result
    = spica::compile(spica::parseCmdlineOption(argc, argv), *argv);

  if (!result)
    return EXIT_FAILURE;

  if (std::holds_alternative<spica::JITResult>(*result))
    return std::get<spica::JITResult>(*result).exit_status;

  // TODO: link
  // system("cc");

  return EXIT_SUCCESS;
}
