cmake_minimum_required(VERSION 3.17)
project(mem_layout C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")

add_executable(mem_layout main.c memlayout.c)

add_executable(mem_1 main.c memlayout.c)
add_executable(mem_2 mem_2.c memlayout.c)
add_executable(mem_3 mem_3.c memlayout.c)
