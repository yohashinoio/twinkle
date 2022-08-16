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

// If the 'system' function could not be run, return std::nullopt; otherwise,
// return linker exit status
[[nodiscard]] std::optional<int>
callLinker(const std::vector<std::filesystem::path>& files)
{
  if (!system(nullptr))
    return std::nullopt;

  std::string command = "cc";

  for (const auto& r : files)
    command += (' ' + r.string());

  return system(command.c_str());
}

int main(const int argc, const char* const* const argv)
{
  const auto context = spica::parseCmdlineOption(argc, argv);

  const auto result = spica::compile(context, *argv);

  if (!result)
    return EXIT_FAILURE;

  if (std::holds_alternative<spica::JITResult>(*result))
    return std::get<spica::JITResult>(*result).exit_status;

  if (context.emit_target == EMIT_EXE_ARG
      && std::holds_alternative<spica::AOTResult>(*result)) {
    const auto& aotresult = std::get<spica::AOTResult>(*result);

    {
      // Call linker
      const auto linker_exit_status = callLinker(aotresult.created_files);

      if (linker_exit_status)
        return *linker_exit_status;
      else {
        std::cerr << "Could not run linker!" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}
