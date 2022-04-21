/**
 * utilities.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <pch/pch.hpp>
#include <support/format.hpp>
#include <support/utils.hpp>

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h> // isatty
#endif

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
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    if (fatal) {
      return format("%s: " COLOR_RED "fatal error: " COLOR_DEFAULT "%s",
                    filename.data(),
                    message.data());
    }
    else {
      return format("%s: " COLOR_RED "error: " COLOR_DEFAULT "%s",
                    filename.data(),
                    message.data());
    }
  }
#endif
  if (fatal)
    return format("%s: fatal error: %s", filename.data(), message.data());

  return format("%s: error: %s", filename.data(), message.data());
}

[[nodiscard]] std::string
formatErrorMessageWithoutFile(const std::string_view message, const bool fatal)
{
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
  if (isatty(fileno(stdout))) {
    if (fatal)
      return format(COLOR_RED "fatal error: " COLOR_DEFAULT "%s",
                    message.data());
    else
      return format(COLOR_RED "error: " COLOR_DEFAULT "%s", message.data());
  }
#endif
  if (fatal)
    return format("fatal error: %s", message.data());

  return format("error: %s", message.data());
}

[[noreturn]] void unreachableInternal(const std::size_t line, const char* file)
{
#ifndef NDEBUG
  if (file) {
    std::cerr << "Unreachable executed"
              << " at " << file << ":" << line << '!' << std::endl;
  }
#endif

#if defined(__GNUC__) // GCC, Clang, ICC
  __builtin_unreachable();
#elif define(_MSC_VER) // MSVC
  __assume(false);
#endif
}

} // namespace maple
