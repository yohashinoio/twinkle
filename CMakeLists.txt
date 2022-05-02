cmake_minimum_required(VERSION 3.16)

project(maple C CXX)

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
# Enable sanitizers
add_compile_options(
  # -fno-omit-frame-pointer
  # -fsanitize=address
  -fsanitize=undefined
)
add_link_options(
  # -fno-omit-frame-pointer
  # -fsanitize=address
  -fsanitize=undefined
)
endif()

add_subdirectory(src)

if(ENABLE_TEST)
  enable_testing()
  add_subdirectory(test)
endif()
