/**
 * common.cpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <codegen/common.hpp>

namespace maple::codegen
{

Variable::Variable(llvm::AllocaInst* pointer,
                   const bool        is_mutable,
                   const bool        is_signed) noexcept
  : pointer{pointer}
  , is_mutable{is_mutable}
  , is_signed{is_signed}
{
}

Value::Value(llvm::Value* value, const bool is_signed) noexcept
  : value{value}
  , is_signed{is_signed}
{
}

Value::Value(llvm::Value* value) noexcept
  : value{value}
  , is_signed{false}
{
}

[[nodiscard]] std::optional<Variable>
SymbolTable::operator[](const std::string& name) const noexcept
try {
  return named_values.at(name);
}
catch (const std::out_of_range&) {
  return std::nullopt;
}

[[nodiscard]] llvm::AllocaInst*
create_entry_block_alloca(llvm::Function*    func,
                          const std::string& var_name,
                          llvm::Type*        type)
{
  return llvm::IRBuilder<>{&func->getEntryBlock(),
                           func->getEntryBlock().begin()}
    .CreateAlloca(type, nullptr, var_name);
}

} // namespace maple::codegen
