/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include "cmd.hpp"
#include <twinkle/compile/compile.hpp>
#include <cstdlib>
#include <iostream>

// If the 'system' function could not be run, return std::nullopt; otherwise,
// return linker exit status
[[nodiscard]] std::optional<int>
callLinker(const std::vector<std::filesystem::path>& files,
           const std::vector<std::string>&           linked_libs)
{
  if (!system(nullptr))
    return std::nullopt;

  std::string command = "gcc";

  for (const auto& r : files)
    command += (' ' + r.string());

  for (const auto& r : linked_libs)
    command += (" -l" + r);

  return system(command.c_str());
}

int main(const int argc, const char* const* const argv)
{
  const auto context = twinkle::parseCmdlineOption(argc, argv);

  const auto result = twinkle::compile(context, *argv);

  if (!result)
    return EXIT_FAILURE;

  if (std::holds_alternative<twinkle::JITResult>(*result))
    return std::get<twinkle::JITResult>(*result).exit_status;

  if (context.emit_target == EMIT_EXE_ARG
      && std::holds_alternative<twinkle::AOTResult>(*result)) {
    const auto& aotresult = std::get<twinkle::AOTResult>(*result);

    {
      // Call linker
      const auto linker_exit_status
        = callLinker(aotresult.created_files, context.linked_libs);

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
