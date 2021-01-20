//
// Created by Michael Kwok on 1/15/21.
//

#include <stdio.h>
#include <stdlib.h>

#include "memlayout.h"

int main() {
    struct memregion *regions = (struct memregion *) malloc(sizeof(struct memregion) * 30);

    int counted = get_mem_layout(regions, 30);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions, counted);

    return 0;
}
