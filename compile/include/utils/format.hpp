/**
 * format.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _38cca214_8d55_11ec_b909_0242ac120002
#define _38cca214_8d55_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <boost/assert.hpp>
#include <type_traits>
#include <memory>

// Converting std::string type to const char* type.
template <typename T,
          typename std::enable_if<
            std::is_same<std::remove_cv_t<std::remove_reference_t<T>>,
                         std::string>::value>::type* = nullptr>
[[nodiscard]] static auto conv_stdstr_to_cstr(T&& value)
{
  return std::forward<T>(value).c_str();
}

// Return non-std::string types as is.
template <typename T,
          typename std::enable_if<
            !std::is_same<std::remove_cv_t<std::remove_reference_t<T>>,
                          std::string>::value>::type* = nullptr>
[[nodiscard]] static auto conv_stdstr_to_cstr(T&& value)
{
  return std::forward<T>(value);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-security"

// format function internal.
template <typename... Args>
[[nodiscard]] static auto format_internal(const std::string& fmt,
                                          Args&&... args)
{
  const int str_len
    = std::snprintf(nullptr, 0, fmt.c_str(), std::forward<Args>(args)...);

  if (str_len < 0) {
    // std::snprintf failed.
    BOOST_ASSERT(0);
  }

  // null termination
  std::size_t buf_size = str_len + sizeof(char);

  std::unique_ptr<char[]> buf(new char[buf_size]);

  std::snprintf(buf.get(), buf_size, fmt.c_str(), args...);

  return std::string(buf.get(), buf.get() + str_len);
}

#pragma GCC diagnostic pop

namespace maple
{

// like c++20 std::format
template <typename... Args>
[[nodiscard]] auto format(const std::string& fmt, Args&&... args)
{
  return format_internal(fmt, conv_stdstr_to_cstr(std::forward<Args>(args))...);
}

} // namespace maple

#endif
