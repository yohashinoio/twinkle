# These codes are licensed under Apache-2.0 License.
# See the LICENSE for details.
# Copyright (c) 2022 Hiramoto Ittou.

set(CMAKE_CXX_STANDARD 20)
set(THREADS_PREFER_PTHREAD_FLAG ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

include(GNUInstallDirs)

# set(CMAKE_C_FLAGS_INIT "-fsanitize=undefined")
# set(CMAKE_CXX_FLAGS_INIT "-fsanitize=undefined")

find_package(Threads REQUIRED)

find_package(Boost REQUIRED COMPONENTS program_options) # 1.71.0 in development
message(STATUS "Using .cmake in: ${Boost_DIR}")

find_package(LLVM REQUIRED CONFIG) # 14.0.0 in development
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using .cmake in: ${LLVM_DIR}")

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})

execute_process(COMMAND llvm-config --libs all
                OUTPUT_VARIABLE CONFIG_OUTPUT)
string(REGEX REPLACE "[ \t]*[\r\n]+[ \t]*" ";" CONFIG_OUTPUT ${CONFIG_OUTPUT})

include_directories(
  ${CMAKE_SOURCE_DIR}/src
  ${Boost_INCLUDE_DIRS}
  ${LLVM_INCLUDE_DIRS}
)

add_executable(
  ${CMAKE_PROJECT_NAME}
  main.cxx
)

target_precompile_headers(${CMAKE_PROJECT_NAME} PRIVATE pch/pch.hxx)

add_subdirectory(ast)
add_subdirectory(codegen)
add_subdirectory(jit)
add_subdirectory(parse)
add_subdirectory(utils)

target_link_libraries(
  ${CMAKE_PROJECT_NAME}
  PRIVATE
  Threads::Threads
  ${Boost_LIBRARIES}
  ${CONFIG_OUTPUT}
  ast
  codegen
  jit
  parse
  utils
)

target_compile_options(
  ${CMAKE_PROJECT_NAME}
  PRIVATE
  -Wall
  -Wextra
  -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING
)

install(
  TARGETS ${CMAKE_PROJECT_NAME}
  RUNTIME
  DESTINATION ${CMAKE_INSTALL_BINDIR}
)
