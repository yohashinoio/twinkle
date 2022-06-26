/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <lapis/mangle/mangler.hpp>
#include <lapis/codegen/type.hpp>
#include <lapis/codegen/common.hpp>

namespace lapis::codegen::mangle
{

[[nodiscard]] std::string
Mangler::mangleFunction(CGContext& ctx, const ast::FunctionDecl& ast) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(ast.accessibility);

  mangled << mangleNamespace(ctx.ns_hierarchy);

  assert(!(ast.is_constructor && ast.is_destructor));

  if (ast.is_constructor)
    mangled << 'C';
  else if (ast.is_destructor)
    mangled << 'D';
  else
    mangled << mangleFunctionName(ast.name.utf8());

  mangled << 'E';

  mangled << mangleParams(ctx, ast.params);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleFunctionCall(CGContext&               ctx,
                            const std::string_view   callee,
                            const std::deque<Value>& args) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << mangleNamespace(ctx.ns_hierarchy);
  mangled << mangleFunctionName(std::string{callee}) << "E";

  mangled << mangleArgs(ctx, args);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleMethod(CGContext&               ctx,
                      const std::string_view   callee,
                      const std::string&       class_name,
                      const std::deque<Value>& args,
                      const Accessibility      accessibility) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(accessibility);

  mangled << mangleNamespace(ctx.ns_hierarchy);
  mangled << mangleFunctionName(std::string{callee}) << "E";

  mangled << mangleThisPointer(ctx, class_name);

  mangled << mangleArgs(ctx, args);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleConstructor(CGContext& ctx, const std::deque<Value>& args) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << mangleNamespace(ctx.ns_hierarchy);
  mangled << 'C' << 'E';

  mangled << mangleArgs(ctx, args);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleDestructor(CGContext& ctx, const std::string& class_name) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << mangleNamespace(ctx.ns_hierarchy);
  mangled << 'D' << 'E';

  mangled << mangleThisPointer(ctx, class_name);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleFunctionName(const std::string& name) const
{
  return boost::lexical_cast<std::string>(name.length()) + name;
}

[[nodiscard]] std::string
Mangler::mangleNamespace(const NsHierarchy& namespaces) const
{
  if (namespaces.empty())
    return {};

  std::ostringstream mangled;

  mangled << 'N';

  for (const auto& r : namespaces)
    mangled << r.name.length() << r.name;

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleThisPointer(CGContext& ctx, const std::string& class_name) const
{
  return PointerType{std::make_shared<UserDefinedType>(class_name)}
    .getMangledName(ctx);
}

[[nodiscard]] std::string
Mangler::mangleArgs(CGContext& ctx, const std::deque<Value>& args) const
{
  std::ostringstream mangled;

  for (const auto& arg : args)
    mangled << arg.getType()->getMangledName(ctx);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleParams(CGContext& ctx, const ast::ParameterList& params) const
{
  std::ostringstream mangled;

  for (const auto& param : *params) {
    if (param.is_vararg)
      mangled << "z";
    else
      mangled << createType(param.type)->getMangledName(ctx);
  }

  return mangled.str();
}

} // namespace lapis::codegen::mangle
