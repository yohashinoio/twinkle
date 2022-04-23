set(LIB_NAME mplc)

set(THREADS_PREFER_PTHREAD_FLAG ON)

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
  ${CMAKE_SOURCE_DIR}/cli/compiler/include
  ${Boost_INCLUDE_DIRS}
  ${LLVM_INCLUDE_DIRS}
)

add_library(
  ${LIB_NAME}
  STATIC
  compile/main.cpp
)

target_precompile_headers(${LIB_NAME} PRIVATE ../include/pch/pch.hpp)

add_subdirectory(codegen)
add_subdirectory(jit)
add_subdirectory(parse)
add_subdirectory(unicode)
add_subdirectory(option)
add_subdirectory(support)

target_link_libraries(
  ${LIB_NAME}
  PRIVATE
  Threads::Threads
  ${Boost_LIBRARIES}
  ${CONFIG_OUTPUT}
  codegen
  jit
  parse
  unicode
  option
  support
)

target_compile_options(
  ${LIB_NAME}
  PRIVATE
  -Wall
  -Wextra
  -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING
)
