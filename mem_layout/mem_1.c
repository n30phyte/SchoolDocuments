/**
 * Created by Michael Kwok on 1/15/21.
 *
 * Driver Program 1: Generate random array heap.
 */

#include <stdio.h>
#include <stdlib.h>

#include "memlayout.h"

int main() {
    struct memregion *regions = (struct memregion *) malloc(sizeof(struct memregion) * 40);

    int counted = get_mem_layout(regions, 20);

    printf("Started with %d regions\n", counted);
    print_mem_layout(regions, 20);

    int *heap_random = (int *) malloc(sizeof(int) * PAGE_SIZE * 4);

    for (int i = 0; i < PAGE_SIZE * 4; i++) {
        heap_random[i] = i;
    }

    printf("After adding heap array:\n");

    struct memregion *regions2 = (struct memregion *) malloc(sizeof(struct memregion) * 40);

    counted = get_mem_layout(regions2, 20);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions2, 20);

    free(heap_random);
    free(regions);
    free(regions2);

    return 0;
}
