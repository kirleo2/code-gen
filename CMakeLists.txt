cmake_minimum_required(VERSION 3.5)
set(CMAKE_CXX_STANDARD 17)

project(tinycc)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/tinycc)

file(GLOB_RECURSE SRC "tinycc/*.cpp" "tinycc/*.h")
add_executable(tinycc ${SRC})

if(MSVC)
  target_compile_options(tinycc PRIVATE /W4 /WX)
else()
  target_compile_options(tinycc PRIVATE -Wall -Wextra -Wpedantic)
endif()


