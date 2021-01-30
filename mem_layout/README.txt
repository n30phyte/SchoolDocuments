CMPUT 379: Assignment 1
mem_layout

Memory scanner for Unix-based operating systems.

There are three drivers provided:

* mem_1
  * Allocates a 500 Megabyte space in the heap with malloc().
  * A new region is created in a higher memory address, showing the addition of a heap.
* mem_2
  * Loads /bin/bash into memory with mmap().
  * An area of memory in the middle resizes, possibly to store the mapped file.
* mem_3
  * Generates the Collatz sequence with a recursive function, to try and get a deep recursive stack.
  * No change was detected, as the stack unwinds for each function return, so nothing would be detected.

Usage instructions:

make will build all 3 drivers,
make mem_N will build driver number N, from 1 to 3.
make clean to clean build artifacts

./mem_N to execute the driver
