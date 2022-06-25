/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _18de40e3_5ff0_4b6c_833a_5f79d5726fcc
#define _18de40e3_5ff0_4b6c_833a_5f79d5726fcc

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <lapis/pch/pch.hpp>
#include <lapis/ast/ast.hpp>

namespace lapis
{

namespace codegen
{

struct NsHierarchy;
struct CGContext;
struct Value;

} // namespace codegen

namespace mangle
{

constexpr const char* ellipsis = "z";

constexpr const char* prefix = "_Z";

struct Mangler : private boost::noncopyable {
  [[nodiscard]] std::string mangleFunction(codegen::CGContext&      ctx,
                                           const ast::FunctionDecl& ast) const;

  [[nodiscard]] std::string
  mangleFunctionCall(codegen::CGContext&               ctx,
                     const std::string_view            callee,
                     const std::deque<codegen::Value>& args) const;

  // The mangled this pointer type is inserted automatically
  [[nodiscard]] std::string
  mangleMethod(codegen::CGContext&               ctx,
               const std::string_view            callee,
               const std::string&                class_name,
               const std::deque<codegen::Value>& args,
               const Accessibility               accessibility) const;

  [[nodiscard]] std::string
  mangleConstructor(codegen::CGContext&               ctx,
                    const std::deque<codegen::Value>& args) const;

  // The mangled this pointer type is inserted automatically
  [[nodiscard]] std::string
  mangleDestructor(codegen::CGContext& ctx,
                   const std::string&  class_name) const;

private:
  [[nodiscard]] std::string mangleFunctionName(const std::string& name) const;

  [[nodiscard]] std::string
  mangleNamespace(const codegen::NsHierarchy& namespaces) const;

  [[nodiscard]] std::string
  mangleThisPointer(codegen::CGContext& ctx,
                    const std::string&  class_name) const;

  [[nodiscard]] std::string
  mangleArgs(codegen::CGContext&               ctx,
             const std::deque<codegen::Value>& args) const;

  [[nodiscard]] std::string
  mangleParams(codegen::CGContext& ctx, const ast::ParameterList& params) const;
};

} // namespace mangle

} // namespace lapis

#endif
