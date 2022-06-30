/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _7955dbec_c131_11ec_9d64_0242ac120002
#define _7955dbec_c131_11ec_9d64_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <rutile/pch/pch.hpp>

namespace rutile
{

namespace program_options = boost::program_options;

[[nodiscard]] program_options::options_description createOptionsDesc();

[[nodiscard]] program_options::variables_map
getVariableMap(const program_options::options_description& desc,
               const int                                   argc,
               const char* const* const                    argv);

// Throws an exception if no input file is passed.
// Note that the return value is a reference.
[[nodiscard]] const std::vector<std::string>&
getInputFiles(const std::string_view                program_name,
              const program_options::variables_map& vmap);

[[nodiscard]] llvm::Reloc::Model
getRelocationModel(const std::string_view                program_name,
                   const program_options::variables_map& vmap);

} // namespace rutile

#endif
