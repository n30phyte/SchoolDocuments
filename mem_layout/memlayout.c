//
// Created by Michael Kwok on 1/15/21.
//
#include "memlayout.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include <signal.h>
#include <setjmp.h>

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
 * Write back original value at @p address,
 * testing write at the same time
 *
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

void save_region(void *start, void *end, unsigned char mode, struct memregion *location) {
    location->from = start;
    location->to = end;
    location->mode = mode;
}

int get_mem_layout(struct memregion *regions, unsigned int size) {
    unsigned long region_count = 0;

    unsigned long end = 0xFFFFF000; // Max address in 32-bit

    struct sigaction sigsegv_handler = {
            .sa_handler = sigaction_handler,
            .sa_flags = 0,
    };

    sigemptyset(&(sigsegv_handler.sa_mask));
    sigaddset(&(sigsegv_handler.sa_mask), SIGSEGV);
    sigaddset(&(sigsegv_handler.sa_mask), SIGBUS);
    sigaction(SIGSEGV, &sigsegv_handler, NULL);
    sigaction(SIGBUS, &sigsegv_handler, NULL);

    void *current_start = 0;
    unsigned char current_mode = MEM_NO;

    // Start scanner
    for (unsigned long current = 0; current < end; current += PAGE_SIZE) {

        char page_mode = MEM_NO;
        char read_val;

        int res = sigsetjmp(segfault_exit, true);

        if (res == 0) {
            // Try read
            read_val = try_read((void *) current);

            // No segfault, at least RO
            page_mode = MEM_RO;

            // Try write
            try_write((void *) current, read_val);

            // No segfault, definitely RW
            page_mode = MEM_RW;
        }

        // Mode switch
        if (page_mode != current_mode) {

            // Still have space in array
            if (size > region_count) {
                save_region(current_start, (void *) (current - 1), current_mode, &regions[region_count]);
                current_start = (void *) current;
            }
            current_mode = page_mode;
            region_count++;
        }
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

        printf("0x%08x-0x%08x %s\n", (int) regions[i].from, (int) regions[i].to, read_write_state);
    }
}