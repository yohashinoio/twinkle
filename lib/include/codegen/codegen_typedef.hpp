/**
 * codegen_typedef.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _f7c47de4_b96b_11ec_8422_0242ac120002
#define _f7c47de4_b96b_11ec_8422_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <pch/pch.hpp>

namespace maple::codegen
{

using CodegenResult
  = std::tuple<std::unique_ptr<llvm::Module>, std::filesystem::path>;

}

#endif
