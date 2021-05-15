add_executable(valws379 main.c lru.c lru.h queue.c queue.h multiset.c multiset.h)

#add_executable(example1 tests/example1.c) # Uninitialized Memory
#add_executable(example2 tests/example2.c) # Use after Free
#add_executable(example3 tests/example3.c) # Out of bounds
#add_executable(example4 tests/example4.c) # Memory Leak
#add_executable(example5 tests/example5.c) # Segfault
#add_executable(lackey tests/lackey.c) # Lackey test

add_executable(heapsort tests/heapsort.c)
add_executable(quicksort tests/quicksort.c)
add_executable(radixsort tests/radixsort.c)
