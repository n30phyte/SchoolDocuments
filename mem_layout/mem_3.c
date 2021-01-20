/**
 * Created by Michael Kwok on 1/15/21.
 *
 * Driver Program 3: Recursion.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include <sys/mman.h>
#include <sys/stat.h>

#include "memlayout.h"

int main() {
    struct memregion *regions = (struct memregion *) malloc(sizeof(struct memregion) * 20);

    int counted = get_mem_layout(regions, 20);

    printf("Started with %d regions\n", counted);
    print_mem_layout(regions, 20);

    struct stat file_stat;

    int fd = open("/bin/bash", O_RDONLY);

    fstat(fd, &file_stat);

    int *fmap = mmap(NULL, file_stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

    printf("\nAfter mmap file:\n");

    struct memregion *regions2 = (struct memregion *) malloc(sizeof(struct memregion) * 20);

    counted = get_mem_layout(regions2, 20);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions2, 20);

    munmap(fmap, file_stat.st_size);

    return 0;
}
