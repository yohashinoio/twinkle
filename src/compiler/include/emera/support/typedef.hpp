/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _39b9a67c_aa6f_11ec_b909_0242ac120002
#define _39b9a67c_aa6f_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <emera/pch/pch.hpp>

namespace emera
{

//===----------------------------------------------------------------------===//
// Using & Typedef
//===----------------------------------------------------------------------===//

using InputIterator
  = boost::u8_to_u32_iterator<std::string::const_iterator, char32_t>;

using PositionCache
  = boost::spirit::x3::position_cache<std::vector<InputIterator>>;

} // namespace emera

#endif
