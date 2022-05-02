/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _aa449b48_c146_11ec_9d64_0242ac120002
#define _aa449b48_c146_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>

namespace maple
{

// Load a file to std::string.
[[nodiscard]] std::string loadFile(const std::string_view       argv_front,
                                   const std::filesystem::path& path);

} // namespace maple

#endif
