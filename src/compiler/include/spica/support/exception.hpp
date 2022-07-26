/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _aaa7cc6a_c130_11ec_9d64_0242ac120002
#define _aaa7cc6a_c130_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <spica/pch/pch.hpp>
#include <stdexcept>

namespace spica
{

struct ErrorBase : public std::runtime_error {
  explicit ErrorBase(const std::string& what_arg)
    : runtime_error{what_arg}
  {
  }
};

} // namespace spica

#endif
