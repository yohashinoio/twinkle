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
#include <string>

namespace miko
{

using iterator_type = std::string::const_iterator;
using position_cache
  = boost::spirit::x3::position_cache<std::vector<iterator_type>>;

namespace parse
{

struct parser {
  parser(iterator_type                first,
         const iterator_type          last,
         const std::filesystem::path& source);

  [[nodiscard]] const ast::program& get_ast() const noexcept;

  [[nodiscard]] const position_cache& get_positions() const noexcept;

private:
  void parse();

  iterator_type       first;
  const iterator_type last;

  ast::program   ast;
  position_cache positions;

  const std::filesystem::path& source;
};

} // namespace parse
} // namespace miko

#endif
