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

namespace mangle
{

constexpr const char* ellipsis = "z";

constexpr const char* prefix = "_Z";

struct Mangler : private boost::noncopyable {
  [[nodiscard]] std::string mangleFunction(CGContext&               ctx,
                                           const ast::FunctionDecl& node) const;

  [[nodiscard]] std::vector<std::string>
  mangleFunctionCall(CGContext&               ctx,
                     const std::string_view   callee,
                     const std::deque<Value>& args) const;

  // The mangled this pointer type is inserted automatically
  [[nodiscard]] std::vector<std::string>
  mangleMethodCall(CGContext&               ctx,
                   const std::string_view   callee,
                   const std::string&       class_name,
                   const std::deque<Value>& args,
                   const Accessibility      accessibility) const;

  // Return results stored in order of priority
  [[nodiscard]] std::vector<std::string>
  mangleConstructorCall(CGContext& ctx, const std::deque<Value>& args) const;

  // The mangled this pointer type is inserted automatically
  [[nodiscard]] std::vector<std::string>
  mangleDestructorCall(CGContext& ctx, const std::string& class_name) const;

private:
  [[nodiscard]] std::string mangleFunctionName(const std::string& name) const;

  // Return results stored in order of priority
  [[nodiscard]] std::vector<std::string>
  mangleNamespace(const NsHierarchy& namespaces) const;

  [[nodiscard]] std::string
  mangleThisPointer(CGContext& ctx, const std::string& class_name) const;

  [[nodiscard]] std::string mangleArgs(CGContext&               ctx,
                                       const std::deque<Value>& args) const;

  [[nodiscard]] std::string
  mangleParams(CGContext& ctx, const ast::ParameterList& params) const;
};

} // namespace mangle

} // namespace codegen

} // namespace lapis

#endif
