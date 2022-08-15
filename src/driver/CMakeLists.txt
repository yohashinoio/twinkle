set(RUNTIME_NAME spica)

include(GNUInstallDirs)

find_package(Threads REQUIRED)

find_package(Boost REQUIRED COMPONENTS program_options) # 1.80.0 in development
message(STATUS "Using .cmake in: ${Boost_DIR}")

include_directories(
  ${Boost_INCLUDE_DIRS}
)

add_executable(
  ${RUNTIME_NAME}
  main.cpp
  cmd.cpp
)

target_link_libraries(
  ${RUNTIME_NAME}
  PRIVATE
  spicac
  Threads::Threads
  ${Boost_LIBRARIES}
  fmt::fmt
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
