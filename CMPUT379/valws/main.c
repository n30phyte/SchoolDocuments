#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

#include "util/lru.h"

const int BUFFER_SIZE = 255;

void collect(int skip, int page, int window) {
    char buffer[BUFFER_SIZE];
    int skipped = 0;

    lru_t *cache = lru_new(window);

    while (fgets(buffer, BUFFER_SIZE, stdin)) {
        // Check that we should process the line.
        if (strpbrk(buffer, "==") == NULL || strpbrk(buffer, "#") == NULL ) {
            if (skipped < skip) {
                skipped++;
                continue;
            }
            strtok(buffer, " ");
            char *address = strtok(NULL, " ");

            long address_numeric = strtol(address, NULL, 16);

            visit(cache, address_numeric / page);

            printf("%zu\n", get_pages(cache));
        }
    }
}

int main(int argc, char *argv[]) {

    int skip_size = 0;

    int opt;
    if ((opt = getopt(argc, argv, "s:")) == 's') {
        skip_size = (int) strtol(optarg, NULL, 10);
    }

    int page_size = (int) strtol(argv[optind], NULL, 10);
    int window_size = (int) strtol(argv[optind + 1], NULL, 10);

    collect(skip_size, page_size, window_size);


}