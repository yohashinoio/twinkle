/**
 * These codes are licensed under Apache-2.0 License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <maple/compile/main.hpp>
#include <filesystem>
#include <sstream>
#include <iostream>
#include <unordered_map>

namespace fs = std::filesystem;

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

  const std::unordered_map<std::string, int> expects{
    {                             "return",  58},
    {                           "addition",  58},
    {                        "subtraction",  58},
    {                     "multiplication",  32},
    {                           "division",  58},
    {                             "modulo",   8},
    {          "four_arithmetic_operators",  41},
    {                        "parentheses",   5},
    {                         "unary_plus",  58},
    {                        "unary_minus",  58},
    {                    "unary_operators",  48},
    {                           "equal_to",   0},
    {                       "not_equal_to",   1},
    {                       "greater_than",   1},
    {                          "less_than",   0},
    {           "greater_than_or_equal_to",   1},
    {              "less_than_or_equal_to",   1},
    {               "expression_statement",  48},
    {               "function_declaration",  58},
    {                           "function",  58},
    {                           "variable",  58},
    {                         "assignment",  58},
    {                                "if1",  58},
    {                                "if2",  58},
    {                                "if3",  58},
    {                               "for1",  58},
    {                               "for2",  58},
    {                               "for3",  58},
    {                               "for4", 110},
    {                               "for5",  58},
    {                            "comment",  58},
    {                   "function_linkage",  58},
    {                       "scalar_types",  58},
    {                         "parameter1",  58},
    {                         "parameter2",  58},
    {                 "integer_conversion",  58},
    {                     "the_void_type1",  58},
    {                     "the_void_type2", 110},
    {                     "string_literal",   0},
    {                  "escape_characters",  58},
    {                      "pointer_types",  58},
    {                        "assignment2",  58},
    {                "addition_assignment",  58},
    {             "subtraction_assignment",  58},
    {          "multiplication_assignment",  58},
    {                "division_assignment",  58},
    {                  "modulo_assignment",   1},
    {                    "boolean_literal",  58},
    {                             "break1",  58},
    {                             "break2",  58},
    {                             "break3",   0},
    {                          "continue1",  58},
    {                          "continue2",  58},
    {                          "continue3",  58},
    {                         "address_of",  58},
    {                              "block",  58},
    {         "octal_hex_escape_character",  58},
    {                              "while",  58},
    {                               "loop",  58},
    {                 "variable_arguments",  58},
    {                       "indirection1",  58},
    {                       "indirection2",  58},
    {                       "indirection3",  58},
    {                  "character_literal",  58},
    {             "indirection_assignment",  58},
    {               "for_loop_declaration",  58},
    {           "init_time_type_inference",  58},
    {              "implicit_conversions1",  41},
    {              "implicit_conversions2",  58},
    {                "integer_literals_32",  58},
    {                "integer_literals_64",  58},
    {                               "sign",  58},
    {                   "prefix_increment",  58},
    {                   "prefix_decrement",  58},
    {                 "for_loop_increment",  58},
    {                 "for_loop_decrement",  58},
    {                         "array_type",   0},
    {                   "initializer_list",   0},
    {              "unicode_char_literal1",  58},
    {              "unicode_char_literal2",   0},
    {                 "unicode_identifier",  58},
    {              "bin_octal_hex_literal",  58},
    {                        "logical_not",  58},
    {                          "subscript",  26},
    {                  "pointer_subscript", 168},
    {                             "sizeof",  58},
    {          "subscript_array_of_signed",  58},
    {   "indirection_to_pointer_to_signed",  58},
    {                 "signed_return_type",  58},
    {        "subscript_array_of_unsigned",  41},
    {  "subscript_pointer_to_signed_array",  58},
    {"subscript_pointer_to_unsigned_array",  41},
    {        "subscript_array_of_pointers",  71},
    { "subscript_pointer_to_signed_array2",  58},
    {                       "return_array",  71},
    {                        "logical_and",  41},
    {                         "logical_or",  58},
    {       "subscript_array_of_unsigned2",  41},
    {                  "struct_definition",  38},
    {                           "pipeline", 116},
    {                          "shadowing",  58},
    {                 "struct_declaration",  41},
    {      "multiple_function_declaration",  58},
    {                       "fopen_fclose",  58},
    {                       "struct_type1",  41},
    {                       "struct_type2",  14},
    {                     "recursive_cast",  58},
    {                 "expr_lhs_subscript",  58},
    {                   "struct_in_struct",  14},
  };

  std::size_t ok_c{};   // ok count.
  std::size_t fail_c{}; // fail count.

  for (const auto& r : fs::recursive_directory_iterator(argv[1])) {
    const char* c_argv[] = {"test", "--JIT", r.path().c_str()};

    // Suppresses compile error output.
    std::cerr.setstate(std::ios::failbit);

    const auto compile_res
      = maple::compile::main(std::extent_v<decltype(c_argv)>, c_argv);

    std::cerr.clear();

    try {
      const auto expect = expects.at(r.path().stem().string());

      if (compile_res.success()) {
        if (*compile_res.getJitResult() == expect) {
          std::cerr << r.path().stem().string() << " => "
                    << *compile_res.getJitResult() << " \x1b[32mOK!\x1b[39m\n";
          ++ok_c;

          continue;
        }
      }

      std::cerr << r.path().stem().string() << " => "
                << *compile_res.getJitResult() << " \x1b[31mFails!\x1b[39m "
                << expect << " expected\n";
      ++fail_c;
    }
    catch (const std::out_of_range&) {
      std::cerr << "No expect is set: " << r.path().stem().string()
                << std::endl;
      std::exit(EXIT_FAILURE);
    }
  }

  std::cerr << "--------------------\n";
  std::cerr << "| \x1b[31mFail\x1b[39m: " << std::setw(10) << fail_c << " |\n";
  std::cerr << "|   \x1b[32mOK\x1b[39m: " << std::setw(10) << ok_c << " |\n";
  std::cerr << "--------------------\n";

  if (fail_c)
    return EXIT_FAILURE;
}
