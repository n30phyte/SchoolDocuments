/**
 * Created by Michael Kwok on 1/15/21.
 *
 * Main program
 */

#include "memlayout.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <setjmp.h>
#include <signal.h>

const unsigned int PAGE_SIZE = USER_PAGE_SIZE;

static sigjmp_buf segfault_exit;

/***
 * Read a single byte from @p address
 *
 * @return Value read from @p address
 */
char try_read(void *address) {
    char p = ((char *) address)[0];
    return p;
}

/***
 * Write back original value at @p address, testing write at the same time
 */
void try_write(void *address, char val) {
    *((char *) address) = val;
}

/**
 * Handler for the thrown signals.
 *
 * @param sig signal code
 */
void sigaction_handler(int sig) {
    siglongjmp(segfault_exit, 1);
}

/**
 * Short helper to store regions into output array.
 */
void save_region(void *start, void *end, unsigned char mode, struct memregion *location) {
    location->from = start;
    location->to = end;
    location->mode = mode;
}

int get_mem_layout(struct memregion *regions, unsigned int size) {
    int region_count = 0;

    struct sigaction sigsegv_handler = {
            .sa_handler = sigaction_handler,
            .sa_flags = 0,
    };

    // Reset sa_mask
    sigemptyset(&(sigsegv_handler.sa_mask));

    // Handle segfaults
    sigaddset(&(sigsegv_handler.sa_mask), SIGSEGV);
    sigaction(SIGSEGV, &sigsegv_handler, NULL);

    // Handle bus errors
    sigaddset(&(sigsegv_handler.sa_mask), SIGBUS);
    sigaction(SIGBUS, &sigsegv_handler, NULL);

    unsigned char current_mode = MEM_NO;

    uint32_t current_start = 0;
    uint32_t end = 0xFFFFFFFF; // Max address in 32-bit
    char page_mode = MEM_NO;

    for (uint64_t current = current_start; current < end; current += PAGE_SIZE) {
        char read_val;

        // Save jump point
        int res = sigsetjmp(segfault_exit, true);

        if (res == 0) {
            page_mode = MEM_NO;
            // Try read
            read_val = try_read((void *) current);

            // No segfault, at least RO
            page_mode = MEM_RO;

            // Try write
            try_write((void *) current, read_val);

            // No segfault, at least RW
            page_mode = MEM_RW;
        }

        // Mode changed
        if (page_mode != current_mode) {

            // Still have space in array
            if (region_count < size) {
                save_region((void *) current_start, (void *) (current - 1),
                            current_mode, &regions[region_count]);
                current_start = current;
            }

            // Update mode
            current_mode = page_mode;
            region_count++;
        }
    }

    // Handle final region
    if (region_count < size) {
        // Still have space in array
        save_region((void *) current_start, (void *) end,
                    current_mode, &regions[region_count]);
        region_count++;
    }

    return region_count;
}

void print_mem_layout(struct memregion *regions, unsigned int size) {

    for (int i = 0; i < size; i++) {
        // Translate from constants to string
        char read_write_state[3];
        switch (regions[i].mode) {
            case MEM_RW:
                strcpy(read_write_state, "RW");
                break;
            case MEM_RO:
                strcpy(read_write_state, "RO");
                break;
            case MEM_NO:
                strcpy(read_write_state, "NO");
                break;
        }

        // Extra entries, don't bother printing and just break out of loop.
        if ((int) regions[i].to == 0 && (int) regions[i].from == 0) {
            break;
        } else {
            printf("0x%08X-0x%08X %s\n", (int) regions[i].from, (int) regions[i].to, read_write_state);
        }
    }

    printf("\n");
}
