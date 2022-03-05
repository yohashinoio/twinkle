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

namespace miko::parse
{

struct parser {
  parser(const std::string& input, const std::filesystem::path& source);

  parser(std::string&& input, const std::filesystem::path& source);

  [[nodiscard]] auto parse() -> ast::program;

private:
  std::string                  input;
  const std::filesystem::path& source;
};

} // namespace miko::parse

#endif
