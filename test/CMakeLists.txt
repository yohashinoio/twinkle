set(RUNTIME_NAME test)

find_package(Threads REQUIRED)

include_directories(
  ${CMAKE_SOURCE_DIR}/src/compiler/include
)

add_executable(
  ${RUNTIME_NAME}
  test.cpp
)

target_link_libraries(
  ${RUNTIME_NAME}
  PRIVATE
  Threads::Threads
  mplc
)

target_compile_options(
  ${RUNTIME_NAME}
  PRIVATE
  -Wall
  -Wextra
)
