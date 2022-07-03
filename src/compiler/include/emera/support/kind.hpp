/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _6bf27b9f_c7c0_48fd_8958_3c3fad1b36ee
#define _6bf27b9f_c7c0_48fd_8958_3c3fad1b36ee

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <emera/pch/pch.hpp>

namespace emera
{

enum class SignKind {
  no_sign, // e.g. array, class
  unsigned_,
  signed_,
};

[[nodiscard]] bool isSigned(const SignKind sk) noexcept;

// Variable qualifier.
enum class VariableQual {
  no_qualifier,
  mutable_,
  reference,
};

enum class Linkage {
  unknown,
  external,
  internal,
};

// Specify to function
enum class Accessibility {
  unknown,
  non_method,
  public_,
  private_,
};

constexpr Accessibility STRUCT_DEFAULT_ACCESSIBILITY = Accessibility::public_;

[[nodiscard]] std::string
getMangledAccessibility(const Accessibility accessibility);

[[nodiscard]] bool isExternallyAccessible(const Accessibility& access) noexcept;

[[nodiscard]] llvm::Function::LinkageTypes
linkageToLLVM(const Linkage linkage) noexcept;

} // namespace emera

#endif
