set(RUNTIME_NAME test)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

find_package(Threads REQUIRED)

include_directories(
  ${CMAKE_SOURCE_DIR}/compile/include
)

add_executable(
  ${RUNTIME_NAME}
  test.cpp
)

target_link_libraries(
  ${RUNTIME_NAME}
  PRIVATE
  Threads::Threads
  compile
)

target_compile_options(
  ${RUNTIME_NAME}
  PRIVATE
  -Wall
  -Wextra
)
