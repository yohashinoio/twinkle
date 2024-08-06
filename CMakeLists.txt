cmake_minimum_required(VERSION 3.16)

project(twinkle C CXX)

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

if(USE_STATIC_LIBS)
  set(Boost_USE_STATIC_LIBS ON)
endif()

add_subdirectory(third-party)

include_directories(
  ${CMAKE_SOURCE_DIR}/src/common
  ${CMAKE_SOURCE_DIR}/src/compiler/include
  ${CMAKE_SOURCE_DIR}/third-party/fmt/include
)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
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
