//
//  parse.hpp
//
//  Copyright (c) 2022 The Miko Authors.
//  MIT License
//

#ifndef _2fdb7c8a_93b7_11ec_b909_0242ac120002
#define _2fdb7c8a_93b7_11ec_b909_0242ac120002

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ast.hpp"
#include "utility.hpp"

namespace miko
{

namespace parse
{

struct parser {
  parser(input_iterator_type                first,
         const input_iterator_type          last,
         const std::filesystem::path& source);

  [[nodiscard]] const ast::program& get_ast() const noexcept;

  [[nodiscard]] const position_cache& get_positions() const noexcept;

private:
  void parse();

  input_iterator_type       first;
  const input_iterator_type last;

  ast::program   ast;
  position_cache positions;

  const std::filesystem::path& source;
};

} // namespace parse
} // namespace miko

#endif