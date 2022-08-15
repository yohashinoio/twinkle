/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include "cmd.hpp"
#include <spica/compile/main.hpp>
#include <cstdlib>
#include <iostream>

int main(const int argc, const char* const* const argv)
{
  const auto context = spica::parseCmdlineOption(argc, argv);

  const auto result = spica::compile(context, *argv);

  if (!result)
    return EXIT_FAILURE;

  if (std::holds_alternative<spica::JITResult>(*result))
    return std::get<spica::JITResult>(*result).exit_status;

  if (context.emit_target == "exe"
      && std::holds_alternative<spica::AOTResult>(*result)) {
    const auto& aotresult = std::get<spica::AOTResult>(*result);

    // Call linker
    std::string command = "cc";
    for (const auto& r : aotresult.created_files)
      command += (' ' + r.string());
    system(command.c_str());
  }

  return EXIT_SUCCESS;
}
