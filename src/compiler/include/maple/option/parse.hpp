/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7955dbec_c131_11ec_9d64_0242ac120002
#define _7955dbec_c131_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <maple/pch/pch.hpp>

namespace maple
{

namespace program_options = boost::program_options;

[[nodiscard]] program_options::options_description createOptionsDesc();

[[nodiscard]] program_options::variables_map
getVariableMap(const program_options::options_description& desc,
               const int                                   argc,
               const char* const* const                    argv);

// If no input files are passed, OptionError is thrown.
// Because the value of the argument is returned by reference, the return value
// is invalid if the argument lifetime has expired.
[[nodiscard]] const std::vector<std::string>&
getInputFiles(const std::string_view                program_name,
              const program_options::variables_map& vmap);

[[nodiscard]] llvm::Reloc::Model
getRelocationModel(const std::string_view                program_name,
                   const program_options::variables_map& vmap);

} // namespace maple

#endif
