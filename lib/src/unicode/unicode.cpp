/**
 * unicode.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <unicode/unicode.hpp>

namespace maple::unicode
{

std::optional<std::string> utf32toUtf8(const char32_t utf32)
{
  std::string utf8;
  utf8.reserve(4);

  if (utf32 < 0x7F)
    utf8.push_back(static_cast<unsigned>(utf32));
  else if (utf32 < 0x7FF) {
    utf8.push_back(0b1100'0000 + static_cast<unsigned>(utf32 >> 6));
    utf8.push_back(0b1000'0000 + static_cast<unsigned>(utf32 & 0b0011'1111));
  }
  else if (utf32 < 0x10000) {
    utf8.push_back(0b1110'0000 + static_cast<unsigned>(utf32 >> 12));
    utf8.push_back(0b1000'0000
                   + static_cast<unsigned>((utf32 >> 6) & 0b0011'1111));
    utf8.push_back(0b1000'0000 + static_cast<unsigned>(utf32 & 0b0011'1111));
  }
  else if (utf32 < 0x110000) {
    utf8.push_back(0b1111'0000 + static_cast<unsigned>(utf32 >> 18));
    utf8.push_back(0b1000'0000
                   + static_cast<unsigned>((utf32 >> 12) & 0b0011'1111));
    utf8.push_back(0b1000'0000
                   + static_cast<unsigned>((utf32 >> 6) & 0b0011'1111));
    utf8.push_back(0b1000'0000 + static_cast<unsigned>(utf32 & 0b0011'1111));
  }
  else
    return std::nullopt;

  return utf8;
}

std::optional<std::string>
utf32toUtf8(const std::u32string_view utf32_str)
{
  std::string utf8_str;

  for (const auto& ch : utf32_str) {
    const auto utf8 = utf32toUtf8(ch);
    if (!utf8)
      return std::nullopt;

    utf8_str += *utf8;
  }

  return utf8_str;
}

} // namespace maple::unicode
