/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _9308967a_c121_11ec_9d64_0242ac120002
#define _9308967a_c121_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <support/exception.hpp>

namespace custard::parse
{

struct ParseError : public ErrorBase {
  explicit ParseError(const std::string& what_arg)
    : ErrorBase{what_arg}
  {
  }
};

} // namespace custard::parse

#endif
