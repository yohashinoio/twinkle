cmake_minimum_required(VERSION 3.16)

project(maple C CXX)

set(CMAKE_CXX_STANDARD 20)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")

# ASAN
add_compile_options(-fno-omit-frame-pointer -fsanitize=address)
add_link_options(-fno-omit-frame-pointer -fsanitize=address)

# UBSAN
add_compile_options(-fsanitize=undefined)
add_link_options(-fsanitize=undefined)

endif()

add_subdirectory(lib)

add_subdirectory(src)

add_subdirectory(test)
