/**
 * Created by Michael Kwok on 1/15/21.
 *
 * Driver Program 1: Generate random array heap.
 */

#include <stdio.h>
#include <stdlib.h>

#include "memlayout.h"

int main() {
    struct memregion *regions = (struct memregion *) malloc(sizeof(struct memregion) * 20);

    int counted = get_mem_layout(regions, 20);

    printf("Started with %d regions\n", counted);
    print_mem_layout(regions, 20);

    int heap_random[4096];

    for (int i = 0; i < 4096; i++) {
        heap_random[i] = rand();
    }

    printf("\nAfter adding heap array:\n");

    struct memregion *regions2 = (struct memregion *) malloc(sizeof(struct memregion) * 20);

    counted = get_mem_layout(regions2, 20);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions2, 20);

    return 0;
}
