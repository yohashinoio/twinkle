/**
 * These codes are licensed under MIT License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include <twk/mangle/mangler.hpp>
#include <twk/codegen/type.hpp>
#include <twk/codegen/common.hpp>

namespace twk::codegen::mangle
{

[[nodiscard]] std::string
Mangler::mangleFunction(const ast::FunctionDecl& decl) const
{
  assert(!decl.isTemplate());

  assert(!(decl.is_constructor && decl.is_destructor));

  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(decl.accessibility);

  mangled << mangleNamespaceHierarchy(ctx.ns_hierarchy).front();

  if (decl.is_constructor)
    mangled << 'C';
  else if (decl.is_destructor)
    mangled << 'D';
  else
    mangled << mangleFunctionName(decl.name.utf8());

  mangled << 'E' << mangleParams(decl.params);

  return mangled.str();
}

[[nodiscard]] std::string Mangler::mangleFunctionTemplate(
  const NamespaceStack&         space,
  const ast::FunctionDecl&      decl,
  const ast::TemplateArguments& template_args) const
{
  assert(!decl.is_constructor && !decl.is_destructor);

  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(decl.accessibility);

  mangled << mangleNamespaceHierarchy(space).front();

  assert(!template_args.types.empty());

  mangled << mangleFunctionName(decl.name.utf8());

  mangled << mangleTemplateArguments(template_args);

  mangled << 'E' << 'T' << mangleParams(decl.params);

  return mangled.str();
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleFunctionTemplateCall(const std::string_view        callee,
                                    const ast::TemplateArguments& template_args,
                                    const std::deque<Value>&      args) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespaceHierarchy(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << mangleFunctionName(std::string{callee});

    mangled << mangleTemplateArguments(template_args);

    mangled << 'E' << 'T' << mangleArgs(args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleFunctionCall(const std::string_view   callee,
                            const std::deque<Value>& args) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespaceHierarchy(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << mangleFunctionName(std::string{callee}) << "E";

    mangled << mangleArgs(args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleMethodCall(const std::string_view   callee,
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

    for (const auto& r : mangleNamespaceHierarchy(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  {
    std::ostringstream mangled;

    mangled << mangleFunctionName(std::string{callee}) << "E";

    mangled << mangleThisPointer(class_name);

    mangled << mangleArgs(args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleConstructorCall(const std::deque<Value>& args) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespaceHierarchy(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << 'C' << 'E' << mangleArgs(args);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleDestructorCall(const std::string& class_name) const
{
  std::vector<std::string> candidates;

  // Previous
  {
    std::ostringstream mangled;

    mangled << prefix;

    for (const auto& r : mangleNamespaceHierarchy(ctx.ns_hierarchy))
      candidates.push_back(mangled.str() + r);
  }

  // Back
  {
    std::ostringstream mangled;

    mangled << 'D' << 'E' << mangleThisPointer(class_name);

    for (auto& r : candidates)
      r += mangled.str();
  }

  return candidates;
}

[[nodiscard]] std::string Mangler::mangleClassTemplateName(
  const std::string&            class_name,
  const ast::TemplateArguments& template_args) const
{
  return class_name + concatTemplateArgs(template_args);
}

[[nodiscard]] std::string
Mangler::mangleTemplateArguments(const ast::TemplateArguments& args) const
{
  std::ostringstream mangled;

  mangled << 'I';

  for (const auto& r : args.types) {
    mangled << createType(ctx, r, ctx.positionOf(args))->getMangledName(ctx);
  }

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleFunctionName(const std::string& name) const
{
  return boost::lexical_cast<std::string>(name.length()) + name;
}

[[nodiscard]] std::vector<std::string>
Mangler::mangleNamespaceHierarchy(const NamespaceStack& namespaces) const
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
Mangler::mangleThisPointer(const std::string& class_name) const
{
  return PointerType{std::make_shared<UserDefinedType>(class_name, false),
                     false}
    .getMangledName(ctx);
}

[[nodiscard]] std::string
Mangler::mangleArgs(const std::deque<Value>& args) const
{
  std::ostringstream mangled;

  for (const auto& arg : args)
    mangled << arg.getType()->getMangledName(ctx);

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleParams(const ast::ParameterList& params) const
{
  std::ostringstream mangled;

  for (const auto& param : *params) {
    if (param.is_vararg)
      mangled << "z";
    else {
      const auto type = createType(ctx, param.type, ctx.positionOf(params));
      mangled << type->getMangledName(ctx);
    }
  }

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::concatTemplateArgs(const ast::TemplateArguments& template_args) const
{
  assert(0 < template_args.types.size());

  std::stringstream ss;

  for (const auto& r : template_args.types) {
    ss
      << '.'
      << createType(ctx, r, ctx.positionOf(template_args))->getMangledName(ctx);
  }

  return ss.str();
}

} // namespace twk::codegen::mangle
