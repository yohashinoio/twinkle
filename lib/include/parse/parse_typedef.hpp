/**
 * parse_typedef.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _e5336816_b96b_11ec_8422_0242ac120002
#define _e5336816_b96b_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>
#include <ast/ast.hpp>
#include <utils/typedef.hpp>

namespace maple::parse
{

using ParseResult
  = std::tuple<ast::Program, PositionCache, std::filesystem::path>;

} // namespace parse

#endif
