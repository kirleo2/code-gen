cmake_minimum_required(VERSION 3.5)
set(CMAKE_CXX_STANDARD 17)

project(tinycc)

# Find LLVM
set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm")
find_package(LLVM REQUIRED CONFIG)

# Include LLVM headers
include_directories(${LLVM_INCLUDE_DIRS})


include_directories(${CMAKE_CURRENT_SOURCE_DIR}/tinycc)

# Ensure LLVM libraries are compiled with the same compiler flags
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

file(GLOB_RECURSE SRC "tinycc/*.cpp" "tinycc/*.h")
add_executable(tinycc ${SRC}
        tinycc/backend/instructions.hpp)

# Link LLVM libraries to the target
llvm_map_components_to_libnames(LLVM_LIBS support core irreader)

target_link_libraries(tinycc ${LLVM_LIBS})

if(MSVC)
  target_compile_options(tinycc PRIVATE /W4 /WX)
else()
  target_compile_options(tinycc PRIVATE -Wall -Wextra -Wpedantic)
endif()


