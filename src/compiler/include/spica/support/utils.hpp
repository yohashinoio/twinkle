/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _d63c66d6_93b5_11ec_b909_0242ac120002
#define _d63c66d6_93b5_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <spica/pch/pch.hpp>
#include <deque>

namespace spica
{

constexpr unsigned int VERSION = 000004;

[[nodiscard]] std::string getVersion();

// With filename.
[[nodiscard]] std::string formatError(const std::string_view filename,
                                      const std::string_view message);

// Without filename.
[[nodiscard]] std::string formatError(const std::string_view message);

[[nodiscard]] std::string stringToLower(const std::string_view str);

[[noreturn]] void unreachableInternal(const std::size_t line, const char* file);

#define unreachable() ::spica::unreachableInternal(__LINE__, __FILE__)

} // namespace spica

#endif
