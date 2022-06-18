/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/mangle/mangler.hpp>
#include <maple/codegen/type.hpp>
#include <maple/codegen/common.hpp>

namespace maple::mangle
{

[[nodiscard]] std::string
Mangler::mangleFunction(codegen::CGContext&      ctx,
                        const ast::FunctionDecl& ast) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(ast.accessibility);

  mangled << mangleNamespace(ctx.namespaces);

  if (ast.is_constructor)
    mangled << "C";
  else
    mangled << mangleFunctionName(ast.name.utf8());

  mangled << "E";

  {
    // Argument types.
    for (const auto& param : *ast.params) {
      if (param.is_vararg)
        mangled << "z";
      else {
        // TODO: Optimization
        mangled << codegen::createType(param.type)->getMangledName();
      }
    }
  }

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleFunctionCall(codegen::CGContext&               ctx,
                            const std::string_view            callee,
                            const std::deque<codegen::Value>& args) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << mangleNamespace(ctx.namespaces);
  mangled << mangleFunctionName(std::string{callee}) << "E";

  for (const auto& arg : args)
    mangled << arg.getType()->getMangledName();

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleMethod(codegen::CGContext&               ctx,
                      const std::string_view            callee,
                      const std::string&                object_name,
                      const std::deque<codegen::Value>& args,
                      const Accessibility               accessibility) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(accessibility);

  mangled << mangleNamespace(ctx.namespaces);
  mangled << mangleFunctionName(std::string{callee}) << "E";

  mangled << mangleThisPointer(object_name);

  for (const auto& arg : args)
    mangled << arg.getType()->getMangledName();

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleConstructor(codegen::CGContext&               ctx,
                           const std::deque<codegen::Value>& args) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << mangleNamespace(ctx.namespaces);
  mangled << 'C' << 'E';

  for (const auto& arg : args)
    mangled << arg.getType()->getMangledName();

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleFunctionName(const std::string& name) const
{
  return boost::lexical_cast<std::string>(name.length()) + name;
}

[[nodiscard]] std::string
Mangler::mangleNamespace(const codegen::NamespaceHierarchy& namespaces) const
{
  if (namespaces.empty())
    return {};

  std::ostringstream mangled;

  mangled << "N";

  for (const auto& r : namespaces)
    mangled << r.name.length() << r.name;

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleThisPointer(const std::string& object_name) const
{
  return codegen::PointerType{
    std::make_shared<codegen::StructType>(object_name)}
    .getMangledName();
}

} // namespace maple::mangle
