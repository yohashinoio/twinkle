set(LIB_NAME twkc)

set(THREADS_PREFER_PTHREAD_FLAG ON)

find_package(Boost REQUIRED COMPONENTS filesystem system) # 1.80.0 in development
message(STATUS "Using .cmake in: ${Boost_DIR}")

include_directories(
  ${Boost_INCLUDE_DIRS}
)

find_package(LLVM REQUIRED CONFIG) # 14 13 in development
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using .cmake in: ${LLVM_DIR}")

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})

set(LLVM_CONFIG_COMMON_OPTION --system-libs --libs all)
set(LLVM_CONFIG_OPTION ${LLVM_CONFIG_COMMON_OPTION})
set(LLVM_CONFIG_OPTION_STATIC --link-static ${LLVM_CONFIG_COMMON_OPTION})

if(LLVM_CONFIG_PATH)
  if(USE_STATIC_LIBS)
    execute_process(COMMAND ${LLVM_CONFIG_PATH} ${LLVM_CONFIG_OPTION_STATIC}
                    OUTPUT_VARIABLE CONFIG_OUTPUT)
  else()
    execute_process(COMMAND ${LLVM_CONFIG_PATH} ${LLVM_CONFIG_OPTION}
                    OUTPUT_VARIABLE CONFIG_OUTPUT)
  endif()
else()
  if(USE_STATIC_LIBS)
    execute_process(COMMAND llvm-config ${LLVM_CONFIG_OPTION_STATIC}
                    OUTPUT_VARIABLE CONFIG_OUTPUT)
  else()
    execute_process(COMMAND llvm-config ${LLVM_CONFIG_OPTION}
                    OUTPUT_VARIABLE CONFIG_OUTPUT)
  endif()
endif()

string(REGEX REPLACE "[ \t]*[\r\n]+[ \t]*" ";" CONFIG_OUTPUT ${CONFIG_OUTPUT})

include_directories(
  ${Boost_INCLUDE_DIRS}
  ${LLVM_INCLUDE_DIRS}
)

add_library(
  ${LIB_NAME}
  STATIC
  compile/compile.cpp
)

target_precompile_headers(${LIB_NAME} PRIVATE ../include/twk/pch/pch.hpp)

add_subdirectory(codegen)
add_subdirectory(jit)
add_subdirectory(mangle)
add_subdirectory(parse)
add_subdirectory(unicode)
add_subdirectory(support)

target_link_libraries(
  ${LIB_NAME}
  PRIVATE
  fmt::fmt
  ${Boost_LIBRARIES}
  ${CONFIG_OUTPUT}
  codegen
  jit
  mangle
  parse
  unicode
  support
)

target_compile_options(
  ${LIB_NAME}
  PRIVATE
  -Wall
  -Wextra
  -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING
)
