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

  mangled << "_Z";

  mangled << mangleNamespace(ctx.namespaces);
  mangled << mangleFunctionName(ast.name.utf8());

  mangled << "E";

  {
    // Argument types.
    for (const auto& param : *ast.params) {
      if (param.is_vararg)
        mangled << "v";
      else
        mangled << param.type->getMangledName();
    }
  }

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleFunctionCall(codegen::CGContext&                ctx,
                            const std::string_view             callee,
                            const std::vector<codegen::Value>& args) const
{
  std::ostringstream mangled;

  mangled << "_Z";

  mangled << mangleNamespace(ctx.namespaces);
  mangled << mangleFunctionName(std::string{callee});

  mangled << "E";

  {
    // Argument types;
    for (const auto& arg : args)
      mangled << arg.getType()->getMangledName();
  }

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
    mangled << r.length() << r;

  return mangled.str();
}

} // namespace maple::mangle
