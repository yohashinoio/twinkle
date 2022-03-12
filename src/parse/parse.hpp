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
#include <util/util.hpp>

namespace miko::parse
{

struct parser {
  parser(std::string&& input, const std::filesystem::path& file_path);

  parser(const std::string& input, const std::filesystem::path& file_path);

  [[nodiscard]] const ast::program& get_ast() const noexcept;

  [[nodiscard]] const position_cache& get_positions() const noexcept;

private:
  void parse();

  std::string               input;
  input_iterator_type       first;
  const input_iterator_type last;

  ast::program   ast;
  position_cache positions;

  const std::filesystem::path& file_path;
};

} // namespace miko::parse

#endif
