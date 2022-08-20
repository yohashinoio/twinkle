/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7955dbec_c131_11ec_9d64_0242ac120002
#define _7955dbec_c131_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <context.hpp>

namespace twinkle
{

[[nodiscard]] Context parseCmdlineOption(const int                argc,
                                         const char* const* const argv);

} // namespace twinkle

#endif
