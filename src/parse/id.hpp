/**
 * id.hpp
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

namespace miko::id
{

enum class type_name {
  i8,
  u8,
  i16,
  u16,
  i32,
  u32,
  i64,
  u64,
  boolean,
};

enum class variable_qualifier {
  mutable_,
};

enum class function_linkage {
  private_,
};

} // namespace miko::id

#endif
