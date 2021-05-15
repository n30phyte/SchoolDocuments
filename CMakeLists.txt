cmake_minimum_required(VERSION 3.16)
project(CMPUT379 C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

add_subdirectory(mem_layout)
add_subdirectory(notapp)
add_subdirectory(valws)