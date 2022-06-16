/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _6bf27b9f_c7c0_48fd_8958_3c3fad1b36ee
#define _6bf27b9f_c7c0_48fd_8958_3c3fad1b36ee

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <maple/pch/pch.hpp>

namespace maple
{

enum class SignKind {
  no_sign, // e.g. array, struct
  unsigned_,
  signed_,
};

[[nodiscard]] bool isSigned(const SignKind sk) noexcept;

// Variable qualifier.
enum class VariableQual {
  no_qualifier,
  mutable_,
};

enum class Linkage {
  unknown,
  external,
  internal,
};

enum class AccessSpecifier {
  unknown,
  public_,
  private_,
};

[[nodiscard]] bool
isExternallyAccessible(const AccessSpecifier& access) noexcept;

[[nodiscard]] llvm::Function::LinkageTypes
linkageToLLVM(const Linkage linkage) noexcept;

} // namespace maple

#endif
