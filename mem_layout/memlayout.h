/**
 * Created by Michael Kwok on 1/15/21.
 *
 * memlayout header
 */

#ifndef _MEMLAYOUT_H_
#define _MEMLAYOUT_H_

// Required to use sigsetjmp from c99 mode
#define _POSIX_C_SOURCE 200112L

// Constants for memory sizes
#define MEM_RW 0
#define MEM_RO 1
#define MEM_NO 2

#ifndef USER_PAGE_SIZE
#define USER_PAGE_SIZE 4096
#endif

extern const unsigned int PAGE_SIZE;

/*
 * Struct to describe regions of memory
 */
struct memregion {
    void *from;
    void *to;
    unsigned char mode; // MEM_RW, or MEM_RO, or MEM_NO
};

/**
 * Gets the memory layout of all addresses from 0x0 to 0xFFFF FFFF
 * @param[out] regions Output array for detected regions within @p size
 * @param[in] size Maximum size of the output array
 * @return Number of found regions
 */
int get_mem_layout(struct memregion *regions, unsigned int size);

/**
 * Prints memory layout in readable format
 * @param[in] regions Input array of @p size with list of regions to print
 * @param[in] size Size of input array
 * @return Number of found regions
 */
void print_mem_layout(struct memregion *regions, unsigned int size);

#endif //_MEMLAYOUT_H_
