/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _0686e754_c146_11ec_9d64_0242ac120002
#define _0686e754_c146_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <spica/support/exception.hpp>

namespace spica
{

struct OptionError : public ErrorBase {
  explicit OptionError(const std::string& what_arg)
    : ErrorBase{what_arg}
  {
  }
};

} // namespace spica

#endif
