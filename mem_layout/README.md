# CMPUT 379: Assignment 1

## mem_layout

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
  * No change was detected, the stack resets after the function is complete, so nothing would be detected.
