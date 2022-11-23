/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _39b9a67c_aa6f_11ec_b909_0242ac120002
#define _39b9a67c_aa6f_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>

namespace twinkle
{

//===----------------------------------------------------------------------===//
// Using & Typedef
//===----------------------------------------------------------------------===//

using InputIterator
  = boost::u8_to_u32_iterator<std::string::const_iterator, char32_t>;

using PositionCache
  = boost::spirit::x3::position_cache<std::vector<InputIterator>>;

using PositionRange = boost::iterator_range<InputIterator>;

} // namespace twinkle

#endif
