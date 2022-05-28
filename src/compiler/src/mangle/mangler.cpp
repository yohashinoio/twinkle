/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/mangle/mangler.hpp>

namespace maple::mangle
{

[[nodiscard]] std::string Mangler::mangle(const std::string_view str)
{
  return fmt::format("_Z{}", str);
}

} // namespace maple::mangle
