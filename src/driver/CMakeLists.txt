set(RUNTIME_NAME maplec)

include(GNUInstallDirs)

include_directories(${CMAKE_SOURCE_DIR}/src/compiler/include)

add_executable(
  ${RUNTIME_NAME}
  main.cpp
)

target_link_libraries(
  ${RUNTIME_NAME}
  PRIVATE
  maplec1
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
