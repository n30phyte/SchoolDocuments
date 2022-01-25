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

    // 500 * 2^20 Bytes of memory = 500 megs
    int *big_heap = (int *) malloc(500 * (1 << 20));

    printf("After adding heap array:\n");

    struct memregion *regions2 = (struct memregion *) malloc(sizeof(struct memregion) * 40);

    counted = get_mem_layout(regions2, 20);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions2, 20);

    free(big_heap);
    free(regions);
    free(regions2);

    return 0;
}
