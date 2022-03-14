/**
 * enum.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _43270eac_a1d2_11ec_b909_0242ac120002
#define _43270eac_a1d2_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

namespace miko
{

enum class variable_qualifier_id {
  mutable_,
};

enum class function_linkage_id {
  private_,
};

} // namespace miko

#endif
