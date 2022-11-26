/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _dc3d87fc_95e2_11ec_b909_0242ac120002
#define _dc3d87fc_95e2_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

//===----------------------------------------------------------------------===//
// LLVM
//===----------------------------------------------------------------------===//

// Used to replace header files depending on llvm version.
#include <llvm/Config/llvm-config.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#if 14 <= LLVM_VERSION_MAJOR
#include <llvm/MC/TargetRegistry.h>
#else
#include <llvm/Support/TargetRegistry.h>
#endif

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/Linker/Linker.h>

//===----------------------------------------------------------------------===//
// Boost
//===----------------------------------------------------------------------===//

#define BOOST_SPIRIT_X3_UNICODE
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/x3/support/ast/variant.hpp>
#include <boost/spirit/home/x3/support/ast/position_tagged.hpp>
#include <boost/spirit/home/x3/support/utility/error_reporting.hpp>
#include <boost/spirit/home/x3/support/utility/annotate_on_success.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/fusion/adapted/struct/adapt_struct.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/noncopyable.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/push_back.hpp>

//===----------------------------------------------------------------------===//
// fmt
//===----------------------------------------------------------------------===//

#include <fmt/color.h>
#include <fmt/core.h>
#include <fmt/ostream.h>

//===----------------------------------------------------------------------===//
// C++ standard
//===----------------------------------------------------------------------===//

#include <filesystem>
#include <string>
#include <iostream>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <fstream>
#include <algorithm>
#include <sstream>
#include <stack>
#include <tuple>
#include <utility>
#include <limits>

//===----------------------------------------------------------------------===//
// C standard
//===----------------------------------------------------------------------===//

#include <cstdint>

#endif
