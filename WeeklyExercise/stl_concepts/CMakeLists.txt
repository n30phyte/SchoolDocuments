cmake_minimum_required(VERSION 3.15)
project(stl_concepts)

set(CMAKE_CXX_STANDARD 11)

add_executable(student student_hash.cpp student_record.cpp)
add_executable(calc complex_postfix.cpp)