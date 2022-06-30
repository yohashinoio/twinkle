/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7eecf126_be3e_11ec_9d64_0242ac120002
#define _7eecf126_be3e_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <rutile/pch/pch.hpp>

namespace rutile::unicode
{

using Codepoint = char32_t;

[[nodiscard]] std::string utf32toUtf8(const char32_t utf32);

[[nodiscard]] std::string utf32toUtf8(const std::u32string_view utf32_str);

} // namespace rutile::unicode

#endif
