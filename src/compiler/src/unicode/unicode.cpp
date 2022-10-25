/**
 * These codes are licensed under LICNSE_NAME License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twinkle/unicode/unicode.hpp>

namespace twinkle::unicode
{

[[nodiscard]] std::string utf32toUtf8(const char32_t utf32)
{
  std::u32string tmp{utf32};

  boost::u32_to_u8_iterator first{cbegin(tmp)}, last{cend(tmp)};

  return std::string(first, last);
}

[[nodiscard]] std::string utf32toUtf8(const std::u32string_view utf32_str)
{
  boost::u32_to_u8_iterator first{cbegin(utf32_str)}, last{cend(utf32_str)};

  return std::string(first, last);
}

[[nodiscard]] std::u32string utf8toUtf32(const std::string_view utf8_str)
{
  boost::u8_to_u32_iterator first{cbegin(utf8_str)}, last{cend(utf8_str)};

  return std::u32string(first, last);
}

} // namespace twinkle::unicode
