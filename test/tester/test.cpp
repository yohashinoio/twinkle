/**
 * These codes are licensed under LGPL-2.1 License
 * See the LICENSE for details
 *
 * Copyright (c) 2022 Hiramoto Ittou
 */

#include "expect.hpp"
#include <twinkle/compile/compile.hpp>
#include <context.hpp>
#include <filesystem>
#include <sstream>
#include <iostream>
#include <unordered_map>
#include <fmt/printf.h>
#include <fmt/color.h>

#define SUPPRESS_COMPILE_ERROR_OUTPUT 0

namespace fs = std::filesystem;

namespace test
{

[[nodiscard]] std::optional<int> runTest(const fs::directory_entry& test_path)
{
#if SUPPRESS_COMPILE_ERROR_OUTPUT
  // Suppresses compile error output
  std::cerr.setstate(std::ios::failbit);
#endif

  if (test_path.is_directory()) {
    std::vector<std::string> paths;

    for (const auto& path : fs::recursive_directory_iterator(test_path))
      paths.push_back(path.path().string());

    const auto result
      = twinkle::compile(twinkle::Context{std::move(paths),
                                          true,
                                          "", // JIT compile, so it's empty
                                          twinkle::DEFAULT_OPT_LEVEL,
                                          "pic",
                                          {},
                                          std::nullopt},
                         "test");

#if SUPPRESS_COMPILE_ERROR_OUTPUT
    std::cerr.clear();
#endif

    if (result)
      return std::get<twinkle::JITResult>(*result).exit_status;
    else
      return std::nullopt;
  }
  else {
    const auto result = twinkle::compile(
      twinkle::Context{std::vector<std::string>{test_path.path()},
                       true,
                       "", // JIT compile, so it's empty
                       twinkle::DEFAULT_OPT_LEVEL,
                       "pic",
                       {},
                       std::nullopt},
      "test");

#if SUPPRESS_COMPILE_ERROR_OUTPUT
    std::cerr.clear();
#endif

    if (result)
      return std::get<twinkle::JITResult>(*result).exit_status;
    else
      return std::nullopt;
  }
}

} // namespace test

int main(const int argc, const char* const* const argv)
{
  if (argc != 2) {
    std::cerr << "Invalid commandline arguments!" << std::endl;
    std::exit(EXIT_FAILURE);
  }
  else if (!fs::is_directory(argv[1])) {
    std::cerr << "No such directory!" << std::endl;
    std::exit(EXIT_FAILURE);
  }

  std::size_t pass_c{};
  std::size_t fail_c{};

  for (const auto& path : fs::directory_iterator(argv[1])) {
    std::cerr << path.path().stem().string();

    const auto result = test::runTest(path);
    const auto expect = test::getExpect(path.path().stem().string());

    if (!expect) {
      fmt::print(stderr,
                 "{} expected end code not set...",
                 path.path().stem().string());
      std::exit(EXIT_FAILURE);
    }

    if (result) {
      if (*result == *expect) {
        std::cerr << " => ";
        fmt::print(stderr,
                   fg(fmt::terminal_color::bright_green),
                   "{} Passed!\n",
                   *result);
        ++pass_c;
        continue;
      }
    }

    std::cerr << " => ";
    fmt::print(stderr,
               fg(fmt::terminal_color::bright_red),
               "{} Failed! ",
               *result);
    fmt::print(stderr, "{} expected\n", *expect);
    ++fail_c;
  }

  std::cerr << "--------------------\n";
  std::cerr << "| " + fmt::format(fg(fmt::terminal_color::bright_red), "Failed")
                 + ": "
            << std::setw(10) << fail_c << " |\n";
  std::cerr << "| "
                 + fmt::format(fg(fmt::terminal_color::bright_green), "Passed")
                 + ": "
            << std::setw(10) << pass_c << " |\n";
  std::cerr << "--------------------\n";

  if (fail_c)
    return EXIT_FAILURE;
}
