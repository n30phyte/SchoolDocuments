# Lackey Working Set Parsing Tool
## CMPUT 379 Winter 2021

To implement the higher quality requirement, a FIFO queue was implemented as a circular buffer, only storing up to `windowsize` items, where each item is the page number accessed. To keep count of the page numbers, a multiset was implemented using an AVL tree, where the page number is the number of nodes in the tree. An LRU Cache-like wrapper over both structures was written to behave the sliding window is expected to. The multiset avoids having to count `windowsize` items each time an addition to the queue is made, which improves performance by a lot. This however still requires a large amount of memory as `windowsize` grows.

### References:
 - AVL Tree based on: https://www.geeksforgeeks.org/avl-tree-set-2-deletion/
