#
#  CMakeLists.txt
#
#  Copyright (c) 2022 The Miko Authors. All rights reserved.
#  MIT License
#

set(CMAKE_CXX_STANDARD 20)
set(THREADS_PREFER_PTHREAD_FLAG ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

find_package(Threads REQUIRED)

find_package(Boost REQUIRED COMPONENTS program_options) # 1.71.0 in development
message(STATUS "Using .cmake in: ${Boost_DIR}")

find_package(LLVM REQUIRED CONFIG) # 13.0.1 in development
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using .cmake in: ${LLVM_DIR}")

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(
  mikoc
  main.cpp
  parse.cpp
  codegen.cpp
  utility.cpp
)

target_precompile_headers(mikoc PRIVATE pch.hpp)

target_include_directories(
  mikoc
  PRIVATE
  ${Boost_INCLUDE_DIRS}
  ${LLVM_INCLUDE_DIRS}
)

llvm_map_components_to_libnames(llvm_libs core support bitwriter)

target_link_libraries(
  mikoc
  PRIVATE
  Threads::Threads
  ${Boost_LIBRARIES}
  ${llvm_libs}
)

target_compile_options(
  mikoc
  PRIVATE
  -Wall
  -Wextra
  -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING
)
