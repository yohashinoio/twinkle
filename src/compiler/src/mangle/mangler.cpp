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

  assert(!(ast.is_constructor && ast.is_destructor));

  if (ast.is_constructor)
    mangled << 'C';
  else if (ast.is_destructor)
    mangled << 'D';
  else
    mangled << mangleFunctionName(ast.name.utf8());

  mangled << 'E';

  {
    for (const auto& param : *ast.params) {
      if (param.is_vararg)
        mangled << "z";
      else
        mangled << codegen::createType(param.type)->getMangledName();
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
                      const std::string&                class_name,
                      const std::deque<codegen::Value>& args,
                      const Accessibility               accessibility) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << getMangledAccessibility(accessibility);

  mangled << mangleNamespace(ctx.namespaces);
  mangled << mangleFunctionName(std::string{callee}) << "E";

  mangled << mangleThisPointer(class_name);

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
Mangler::mangleDestructor(codegen::CGContext& ctx,
                          const std::string&  class_name) const
{
  std::ostringstream mangled;

  mangled << prefix;

  mangled << mangleNamespace(ctx.namespaces);
  mangled << 'D' << 'E';

  mangled << mangleThisPointer(class_name);

  std::cout << mangled.str() << std::endl;

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

  mangled << 'N';

  for (const auto& r : namespaces)
    mangled << r.name.length() << r.name;

  return mangled.str();
}

[[nodiscard]] std::string
Mangler::mangleThisPointer(const std::string& class_name) const
{
  return codegen::PointerType{std::make_shared<codegen::StructType>(class_name)}
    .getMangledName();
}

} // namespace maple::mangle
