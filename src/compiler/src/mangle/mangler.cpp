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
Mangler::mangleFunction(CGContext& ctx, const ast::FunctionDecl& node) const
{
  assert(!(node.is_constructor && node.is_destructor));

  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(node.accessibility);

  mangled << mangleNamespace(ctx.ns_hierarchy).front();

  if (node.is_constructor)
    mangled << 'C';
  else if (node.is_destructor)
    mangled << 'D';
  else
    mangled << mangleFunctionName(node.name.utf8());

  mangled << 'E' << mangleParams(ctx, node.params);

  return mangled.str();
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleFunctionCall(CGContext&               ctx,
                            const std::string_view   callee,
                            const std::deque<Value>& args) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespace(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << mangleFunctionName(std::string{callee}) << "E";

    mangled << mangleArgs(ctx, args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleMethodCall(CGContext&               ctx,
                          const std::string_view   callee,
                          const std::string&       class_name,
                          const std::deque<Value>& args,
                          const Accessibility      accessibility) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    mangled << getMangledAccessibility(accessibility);

    for (const auto& r : mangleNamespace(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  {
    std::ostringstream mangled;

    mangled << mangleFunctionName(std::string{callee}) << "E";

    mangled << mangleThisPointer(ctx, class_name);

    mangled << mangleArgs(ctx, args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleConstructorCall(CGContext&               ctx,
                               const std::deque<Value>& args) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespace(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << 'C' << 'E' << mangleArgs(ctx, args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleDestructorCall(CGContext&         ctx,
                              const std::string& class_name) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespace(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << 'D' << 'E' << mangleThisPointer(ctx, class_name);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::string
Mangler::mangleFunctionName(const std::string& name) const
{
  return boost::lexical_cast<std::string>(name.length()) + name;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleNamespace(const NsHierarchy& namespaces) const
{
  std::vector<std::string> candidates;

  std::ostringstream mangled;

  candidates.push_back(mangled.str());

  mangled << 'N';

  for (const auto& r : namespaces) {
    mangled << r.name.length() << r.name;
    candidates.push_back(mangled.str());
  }

  // Reverse because priorities are reversed
  std::reverse(candidates.begin(), candidates.end());

  return candidates;
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
