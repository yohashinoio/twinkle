/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <pch/pch.hpp>
#include <support/utils.hpp>

namespace maple
{

std::string getVersion()
{
  const auto major = MAPLE_VER / 100000;
  const auto minor = MAPLE_VER / 100 % 1000;
  const auto patch = MAPLE_VER % 100;

  return boost::lexical_cast<std::string>(major) + '.'
         + boost::lexical_cast<std::string>(minor) + '.'
         + boost::lexical_cast<std::string>(patch);
}

[[nodiscard]] std::string formatErrorMessage(const std::string_view filename,
                                             const std::string_view message,
                                             const bool             fatal)
{
  if (fatal) {
    return fmt::format("{}: ", filename)
           + fmt::format(fg(fmt::color::red), "fatal error: ")
           + std::string(message);
  }
  else {
    return fmt::format("{}: ", filename)
           + fmt::format(fg(fmt::color::red), "error: ") + std::string(message);
  }
}

[[nodiscard]] std::string
formatErrorMessageWithoutFile(const std::string_view message, const bool fatal)
{
  if (fatal) {
    return fmt::format(fg(fmt::color::red), "fatal error: ")
           + std::string(message);
  }
  else
    return fmt::format(fg(fmt::color::red), "error: ") + std::string(message);
}

[[nodiscard]] std::string stringToLower(const std::string_view str)
{
  std::string result;

  std::transform(str.begin(),
                 str.end(),
                 std::back_inserter(result),
                 [](auto&& ch) { return std::tolower(ch); });

  return result;
}

[[noreturn]] void unreachableInternal(const std::size_t line, const char* file)
{
#ifndef NDEBUG
  if (file)
    fmt::print(stderr, "Unreachable executed at {} : {}!\n", file, line);
#endif

#if defined(__GNUC__) // GCC, Clang, ICC
  __builtin_unreachable();
#elif define(_MSC_VER) // MSVC
  __assume(false);
#endif
}

} // namespace maple
