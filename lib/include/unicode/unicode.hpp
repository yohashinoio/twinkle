/**
 * unicode.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7eecf126_be3e_11ec_9d64_0242ac120002
#define _7eecf126_be3e_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>

// In this program, code points are treated as std::uint32_t and encoded ones as
// char32_t

namespace maple::unicode
{

using Codepoint = std::uint32_t;

// If conversion fails, std::nullopt returns.
std::optional<std::string> charUTF32toUTF8(const char32_t utf32);

} // namespace maple::unicode

#endif
