/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _d63c66d6_93b5_11ec_b909_0242ac120002
#define _d63c66d6_93b5_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <deque>

namespace custard
{

constexpr unsigned int CUSTARD_VER = 100000;

[[nodiscard]] std::string getVersion();

// With filename.
[[nodiscard]] std::string formatError(const std::string_view filename,
                                      const std::string_view message);

// Without filename.
[[nodiscard]] std::string formatError(const std::string_view message);

[[nodiscard]] std::string stringToLower(const std::string_view str);

/*
  After stacking the argument 1, start stacking from the left of the
  argument 2.
  Example: (4, 8, 1, 0)
  |=--|
  | 0 | <- top
  | 1 |
  | 8 |
  | 4 |
  |---|
*/
template <typename T, typename... Ts>
[[nodiscard]] auto createStack(T&& arg, Ts&&... args)
{
  return std::stack{
    std::deque{std::forward<T>(arg), std::forward<Ts>(args)...}
  };
}

[[noreturn]] void unreachableInternal(const std::size_t line, const char* file);

#define unreachable() ::custard::unreachableInternal(__LINE__, __FILE__)

} // namespace custard

#endif
