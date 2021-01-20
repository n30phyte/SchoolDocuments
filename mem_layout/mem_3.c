/**
 * Created by Michael Kwok on 1/15/21.
 *
 * Driver Program 3: Recursion.
 */

#include <stdio.h>
#include <stdlib.h>

#include "memlayout.h"

/**
 * Simple implementation for calculating the sequence of Collatz's conjecture.
 */
int collatz(int n) {
    printf("%d ", n);
    if (n == 1) {
        return 1;
    } else if (n % 2 == 0) {
        return collatz(n / 2);
    } else {
        return collatz((3 * n) + 1);
    }
}

int main() {
    struct memregion *regions = (struct memregion *) malloc(sizeof(struct memregion) * 20);

    int counted = get_mem_layout(regions, 20);

    printf("Started with %d regions\n", counted);
    print_mem_layout(regions, 20);

    collatz(63728127); // Try to get many recursive frames deep

    printf("\nAfter collatz:\n");

    struct memregion *regions2 = (struct memregion *) malloc(sizeof(struct memregion) * 20);

    counted = get_mem_layout(regions2, 20);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions2, 20);

    return 0;
}
