/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#ifndef _18de40e3_5ff0_4b6c_833a_5f79d5726fcc
#define _18de40e3_5ff0_4b6c_833a_5f79d5726fcc

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <twinkle/pch/pch.hpp>
#include <twinkle/ast/ast.hpp>

namespace twinkle
{

namespace codegen
{

struct NamespaceStack;
struct CGContext;
struct Value;
struct Type;

namespace mangle
{

constexpr const char* ellipsis = "z";

constexpr const char* prefix = "_Z";

struct Mangler : private boost::noncopyable {
  explicit Mangler(CGContext& ctx) noexcept
    : ctx{ctx}
  {
  }

  [[nodiscard]] std::string mangleFunction(const ast::FunctionDecl& decl) const;

  [[nodiscard]] std::string
  mangleFunctionTemplate(const NamespaceStack&         space,
                         const ast::FunctionDecl&      decl,
                         const ast::TemplateArguments& template_args) const;

  [[nodiscard]] std::vector<std::string>
  mangleFunctionCall(const std::string_view   callee,
                     const std::deque<Value>& args) const;

  [[nodiscard]] std::vector<std::string>
  mangleFunctionTemplateCall(const std::string_view        callee,
                             const ast::TemplateArguments& template_args,
                             const std::deque<Value>&      args) const;

  // The mangled this pointer type is inserted automatically
  [[nodiscard]] std::vector<std::string>
  mangleMethodCall(const std::string_view   callee,
                   const std::string&       class_name,
                   const std::deque<Value>& args,
                   const Accessibility      accessibility) const;

  // Return results stored in order of priority
  [[nodiscard]] std::vector<std::string>
  mangleConstructorCall(const std::deque<Value>& args) const;

  // The mangled this pointer type is inserted automatically
  [[nodiscard]] std::vector<std::string>
  mangleDestructorCall(const std::string& class_name) const;

  [[nodiscard]] std::string
  mangleClassTemplateName(const std::string&            class_name,
                          const ast::TemplateArguments& template_args) const;

private:
  [[nodiscard]] std::string
  mangleTemplateArguments(const ast::TemplateArguments& args) const;

  [[nodiscard]] std::string mangleFunctionName(const std::string& name) const;

  // Return results stored in order of priority
  [[nodiscard]] std::vector<std::string>
  mangleNamespaceHierarchy(const NamespaceStack& namespaces) const;

  [[nodiscard]] std::string
  mangleThisPointer(const std::string& class_name) const;

  [[nodiscard]] std::string mangleArgs(const std::deque<Value>& args) const;

  [[nodiscard]] std::string
  mangleParams(const ast::ParameterList& params) const;

  // <A, B> to ".A.B"
  [[nodiscard]] std::string
  concatTemplateArgs(const ast::TemplateArguments& template_args) const;

  CGContext& ctx;
};

} // namespace mangle

} // namespace codegen

} // namespace twinkle

#endif
