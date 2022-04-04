cmake_minimum_required(VERSION 3.16)

project(miko C CXX)

set(CMAKE_CXX_STANDARD 17)

add_subdirectory(frontend)

add_subdirectory(driver)
