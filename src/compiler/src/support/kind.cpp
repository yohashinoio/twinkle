/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <twinkle/support/kind.hpp>
#include <twinkle/support/utils.hpp>

namespace twinkle
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

} // namespace twinkle
