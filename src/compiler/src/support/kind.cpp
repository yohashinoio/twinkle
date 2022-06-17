/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/support/kind.hpp>
#include <maple/support/utils.hpp>

namespace maple
{

[[nodiscard]] bool isSigned(const SignKind sk) noexcept
{
  switch (sk) {
  case SignKind::no_sign:
    unreachable();
  case SignKind::signed_:
    return true;
  case SignKind::unsigned_:
    return false;
  }

  unreachable();
}

[[nodiscard]] llvm::Function::LinkageTypes
linkageToLLVM(const Linkage linkage) noexcept
{
  switch (linkage) {
  case Linkage::unknown:
    unreachable();
  case Linkage::external:
    return llvm::Function::LinkageTypes::ExternalLinkage;
  case Linkage::internal:
    return llvm::Function::LinkageTypes::InternalLinkage;
  }

  unreachable();
}

[[nodiscard]] std::string
getMangledAccessibility(const Accessibility accessibility)
{
  switch (accessibility) {
  case Accessibility::private_:
    return "P";
  default:
    return "";
  }

  unreachable();
}

[[nodiscard]] bool isExternallyAccessible(const Accessibility& access) noexcept
{
  switch (access) {
  case Accessibility::public_:
    return true;
  case Accessibility::private_:
  case Accessibility::non_method:
    return false;
  case Accessibility::unknown:
    unreachable();
  }

  unreachable();
}

} // namespace maple
