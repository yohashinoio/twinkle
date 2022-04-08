/**
 * parse.hpp
 *
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#ifndef _2fdb7c8a_93b7_11ec_b909_0242ac120002
#define _2fdb7c8a_93b7_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <ast/ast.hpp>
#include <utils/util.hpp>
#include <utils/typedef.hpp>

namespace maple::parse
{

struct Parser {
  Parser(std::string&&                input,
         const std::filesystem::path& file_path,
         const bool                   error_output);

  Parser(const std::string&           input,
         const std::filesystem::path& file_path,
         const bool                   error_output);

  [[nodiscard]] const ast::Program& get_ast() const noexcept;

  [[nodiscard]] const PositionCache& get_positions() const noexcept;

private:
  void parse();

  std::string         input;
  InputIterator       first;
  const InputIterator last;

  ast::Program  ast;
  PositionCache positions;

  const std::filesystem::path& file_path;
};

} // namespace maple::parse

#endif
