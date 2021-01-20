/**
 * Created by Michael Kwok on 1/15/21.
 *
 * Driver Program 2: mmap /usr/bin/bash.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include <sys/mman.h>
#include <sys/stat.h>

#include "memlayout.h"

int main() {
    struct memregion *regions = (struct memregion *) malloc(sizeof(struct memregion) * 30);

    int counted = get_mem_layout(regions, 20);

    printf("Started with %d regions\n", counted);
    print_mem_layout(regions, 20);

    struct stat file_stat;

    int fd = open("/bin/bash", O_RDONLY);

    fstat(fd, &file_stat);

    int *fmap = mmap(NULL, file_stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

    printf("After mmap file:\n");

    struct memregion *regions2 = (struct memregion *) malloc(sizeof(struct memregion) * 30);

    counted = get_mem_layout(regions2, 20);

    printf("Found %d regions\n", counted);
    print_mem_layout(regions2, 20);

    munmap(fmap, file_stat.st_size);

    free(regions);
    free(regions2);

    return 0;
}
