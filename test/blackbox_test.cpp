/**
 * These codes are licensed under LICNSE_NAME License.
 * See the LICENSE for details.
 *
 * Copyright (c) 2022 Hiramoto Ittou.
 */

#include <lapis/compile/main.hpp>
#include <filesystem>
#include <sstream>
#include <iostream>
#include <unordered_map>
#include <fmt/printf.h>
#include <fmt/color.h>

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

  const std::unordered_map<std::string, int> expected_retcodes{
    {                                  "return",  58},
    {                                "addition",  58},
    {                             "subtraction",  58},
    {                          "multiplication",  32},
    {                                "division",  58},
    {                                  "modulo",   8},
    {               "four_arithmetic_operators",  41},
    {                             "parentheses",   5},
    {                              "unary_plus",  58},
    {                             "unary_minus",  58},
    {                         "unary_operators",  48},
    {                                "equal_to",  14},
    {                            "not_equal_to",   1},
    {                            "greater_than",   1},
    {                               "less_than",  14},
    {                "greater_than_or_equal_to",   1},
    {                   "less_than_or_equal_to",   1},
    {                    "expression_statement",  48},
    {                    "function_declaration",  58},
    {                                "function",  58},
    {                                "variable",  58},
    {                              "assignment",  58},
    {                                     "if1",  58},
    {                                     "if2",  58},
    {                                     "if3",  58},
    {                                    "for1",  58},
    {                                    "for2",  58},
    {                                    "for3",  58},
    {                                    "for4", 110},
    {                                    "for5",  58},
    {                                 "comment",  58},
    {                        "function_linkage",  58},
    {                            "scalar_types",  58},
    {                              "parameter1",  58},
    {                              "parameter2",  58},
    {                      "integer_conversion",  58},
    {                          "the_void_type1",  58},
    {                          "the_void_type2", 110},
    {                          "string_literal",  14},
    {                       "escape_characters",  58},
    {                           "pointer_types",  58},
    {                             "assignment2",  58},
    {                     "addition_assignment",  58},
    {                  "subtraction_assignment",  58},
    {               "multiplication_assignment",  58},
    {                     "division_assignment",  58},
    {                       "modulo_assignment",   1},
    {                         "boolean_literal",  58},
    {                                  "break1",  58},
    {                                  "break2",  58},
    {                                  "break3",  14},
    {                               "continue1",  58},
    {                               "continue2",  58},
    {                               "continue3",  58},
    {                              "address_of",  58},
    {                                   "block",  58},
    {              "octal_hex_escape_character",  58},
    {                                   "while",  58},
    {                                    "loop",  58},
    {                      "variable_arguments",  58},
    {                            "dereference1",  58},
    {                            "dereference2",  58},
    {                            "dereference3",  58},
    {                       "character_literal",  58},
    {                  "dereference_assignment",  58},
    {                    "for_loop_declaration",  58},
    {                "init_time_type_inference",  58},
    {                   "implicit_conversions1",  41},
    {                   "implicit_conversions2",  58},
    {                     "integer_literals_32",  58},
    {                     "integer_literals_64",  58},
    {                                    "sign",  58},
    {                        "prefix_increment",  58},
    {                        "prefix_decrement",  58},
    {                      "for_loop_increment",  58},
    {                      "for_loop_decrement",  58},
    {                              "array_type",  14},
    {                   "unicode_char_literal1",  58},
    {                   "unicode_char_literal2",  41},
    {                      "unicode_identifier",  58},
    {                   "bin_octal_hex_literal",  58},
    {                             "logical_not",  58},
    {                               "subscript",  26},
    {                       "pointer_subscript", 168},
    {                                  "sizeof",  58},
    {               "subscript_array_of_signed",  58},
    {        "dereference_to_pointer_to_signed",  58},
    {                      "signed_return_type",  58},
    {             "subscript_array_of_unsigned",  41},
    {       "subscript_pointer_to_signed_array",  58},
    {     "subscript_pointer_to_unsigned_array",  41},
    {             "subscript_array_of_pointers",  71},
    {      "subscript_pointer_to_signed_array2",  58},
    {                            "return_array",  71},
    {                             "logical_and",  41},
    {                              "logical_or",  58},
    {            "subscript_array_of_unsigned2",  41},
    {                       "struct_definition",  38},
    {                                "pipeline", 116},
    {                               "shadowing",  58},
    {                      "struct_declaration",  41},
    {           "multiple_function_declaration",  58},
    {                            "fopen_fclose",  58},
    {                            "struct_type1",  41},
    {                            "struct_type2",  14},
    {                          "recursive_cast",  58},
    {                      "expr_lhs_subscript",  58},
    {                        "struct_in_struct",  14},
    {                           "member_access",  58},
    {                        "assign_to_member",  58},
    {          "struct_in_struct_member_access",  58},
    {                    "function_overloading",  58},
    {      "dereference_to_pointer_to_unsigned",  14},
    {                 "access_to_signed_member",  14},
    {               "access_to_unsigned_member",  14},
    {                     "floating_point_type",  58},
    {                 "floating_point_addition",  14},
    {              "floating_point_subtraction",  14},
    {           "floating_point_multiplication",  41},
    {                 "floating_point_division",  41},
    {                   "floating_point_modulo",  58},
    {                     "floating_point_cast",  58},
    {      "floating_point_addition_assignment",  14},
    {   "floating_point_subtraction_assignment",  14},
    {"floating_point_multiplication_assignment",  41},
    {      "floating_point_division_assignment",  41},
    {        "floating_point_modulo_assignment",  58},
    {          "floating_point_cast_assignment",  58},
    {    "floating_point_relational_operations",  14},
    {                      "struct_methods_def", 116},
    {                     "struct_methods_call",  58},
    {                            "this_pointer",  14},
    {         "reference_member_var_in_methods",  58},
    {            "reference_methods_in_methods",  58},
    {              "multidimensional_array_def",  58},
    {                        "pointer_to_array",  58},
    {                         "private_methods",  58},
    {    "refers_to_declared_undefined_methods",  58},
    {          "constructors_and_class_literal", 100},
    {                    "constructor_overload",  58},
    {                              "destructor",  58},
    {                 "typedef_primitive_types",  58},
    {                   "typedef_pointer_types",  58},
    {                    "typedef_struct_types",  58},
    {                     "typedef_array_types",  13},
    {                       "typedef_void_type",  58},
    {            "typedef_floating_point_types",  58},
    {                           "array_literal", 100},
    {                 "floating_point_negative",  58},
    {  "constructor_call_of_same_class_as_self",  58},
  };

  std::size_t ok_c{};   // ok count.
  std::size_t fail_c{}; // fail count.

  for (const auto& r : fs::recursive_directory_iterator(argv[1])) {
    const char* c_argv[] = {"test", "--JIT", r.path().c_str()};

    std::cerr << r.path().stem().string();

    // Suppresses compile error output.
    std::cerr.setstate(std::ios::failbit);

    const auto result
      = lapis::compile::main(std::extent_v<decltype(c_argv)>, c_argv);

    std::cerr.clear();

    try {
      const auto expected_retcode
        = expected_retcodes.at(r.path().stem().string());

      if (result.success()) {
        if (*result.getJitResult() == expected_retcode) {
          std::cerr << " => ";
          fmt::print(stderr,
                     fg(fmt::terminal_color::bright_green),
                     "{} OK!\n",
                     *result.getJitResult());
          ++ok_c;
          continue;
        }
      }

      std::cerr << " => ";
      fmt::print(stderr,
                 fg(fmt::terminal_color::bright_red),
                 "{} Fails! ",
                 *result.getJitResult());
      fmt::print(stderr, "{} expected\n", expected_retcode);
      ++fail_c;
    }
    catch (const std::out_of_range&) {
      fmt::print(stderr,
                 "{} expected end code not set...",
                 r.path().stem().string());
      std::exit(EXIT_FAILURE);
    }
  }

  std::cerr << "--------------------\n";
  std::cerr << "| " + fmt::format(fg(fmt::terminal_color::bright_red), "Fail")
                 + ": "
            << std::setw(10) << fail_c << " |\n";
  std::cerr << "|   " + fmt::format(fg(fmt::terminal_color::bright_green), "OK")
                 + ": "
            << std::setw(10) << ok_c << " |\n";
  std::cerr << "--------------------\n";

  if (fail_c)
    return EXIT_FAILURE;
}
