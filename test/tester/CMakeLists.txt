set(RUNTIME_NAME blackbox_test)

find_package(Threads REQUIRED)

include_directories(
  ${CMAKE_SOURCE_DIR}/src/compiler/include
  ${CMAKE_SOURCE_DIR}/third-party/fmt/include
)

add_executable(
  ${RUNTIME_NAME}
  tester.cpp
  expect.cpp
)

target_link_libraries(
  ${RUNTIME_NAME}
  PRIVATE
  Threads::Threads
  fmt::fmt
  emera
)

target_compile_options(
  ${RUNTIME_NAME}
  PRIVATE
  -Wall
  -Wextra
)

add_test(
  NAME blackbox_testing
  COMMAND $<TARGET_FILE:blackbox_test> ${CMAKE_SOURCE_DIR}/test/cases
)
