cmake_minimum_required(VERSION 3.16)
project(ECE420 C)

set(CMAKE_C_STANDARD 11)

find_package(Threads REQUIRED)
find_package(OpenMP REQUIRED)

add_executable(Barrier Assignment1/barrier.c)
add_executable(ProdCon Assignment1/prodcon.c)

target_link_libraries(Barrier PRIVATE Threads::Threads)
target_link_libraries(ProdCon PRIVATE Threads::Threads)

add_executable(OmpThreads Assignment2/threads.c)

target_link_libraries(OmpThreads PRIVATE OpenMP::OpenMP_C)

