/**
 * These codes are licensed under MIT License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _7eecf126_be3e_11ec_9d64_0242ac120002
#define _7eecf126_be3e_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twk/pch/pch.hpp>

namespace twk::unicode
{

using Codepoint = char32_t;

[[nodiscard]] std::string utf32toUtf8(const char32_t utf32);

[[nodiscard]] std::string utf32toUtf8(const std::u32string_view utf32_str);

[[nodiscard]] std::u32string utf8toUtf32(const std::string_view utf8_str);

} // namespace twk::unicode

#endif
