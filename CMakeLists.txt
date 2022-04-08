cmake_minimum_required(VERSION 3.16)

project(maple C CXX)

set(CMAKE_CXX_STANDARD 20)

add_subdirectory(compile)

add_subdirectory(driver)

add_subdirectory(tests)
