/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <spica/support/file.hpp>
#include <spica/support/utils.hpp>

namespace spica
{

// Load a file to std::string.
[[nodiscard]] std::string loadFile(const std::string_view       program_name,
                                   const std::filesystem::path& path)
{
  if (!std::filesystem::exists(path)) {
    throw FileError{
      formatError(program_name,
                  fmt::format("{}: No such file or directory", path.string()))};
  }

  if (auto file = std::ifstream{path, std::ios_base::binary}) {
    std::stringstream ss;
    ss << file.rdbuf();
    return ss.str();
  }

  throw FileError{
    formatError(program_name,
                fmt::format("{}: Could not open file", path.string()))};
}

} // namespace spica
