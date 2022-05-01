/**
 * exception.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _aaa7cc6a_c130_11ec_9d64_0242ac120002
#define _aaa7cc6a_c130_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <stdexcept>

namespace maple
{

struct ErrorBase : public std::runtime_error {
  explicit ErrorBase(const std::string& what_arg)
    : runtime_error{what_arg}
  {
  }
};

} // namespace maple

#endif