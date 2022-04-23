set(RUNTIME_NAME maplec)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

include(GNUInstallDirs)

include_directories(${CMAKE_SOURCE_DIR}/cli/compiler/include)

add_executable(
  ${RUNTIME_NAME}
  main.cpp
)

target_link_libraries(
  ${RUNTIME_NAME}
  PRIVATE
  mplc
)

target_compile_options(
  ${RUNTIME_NAME}
  PRIVATE
  -Wall
  -Wextra
)

install(
  TARGETS ${RUNTIME_NAME}
  RUNTIME
  DESTINATION ${CMAKE_INSTALL_BINDIR}
)
