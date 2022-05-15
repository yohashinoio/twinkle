/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <support/file.hpp>
#include <support/utils.hpp>
#include <support/exception.hpp>

namespace custard
{

// Exception class for errors related to file operations.
struct FileError : public ErrorBase {
  explicit FileError(const std::string& what_arg)
    : ErrorBase{what_arg}
  {
  }
};

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

} // namespace custard
