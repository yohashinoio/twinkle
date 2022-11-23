/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _b3250e7f_1a51_48cc_9561_01f0d5670e26
#define _b3250e7f_1a51_48cc_9561_01f0d5670e26

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>
#include <optional>

namespace test
{

[[nodiscard]] std::optional<int> getExpect(const std::string& test_name);

} // namespace test

#endif
