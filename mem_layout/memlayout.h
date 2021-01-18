//
// Created by Michael Kwok on 1/15/21.
//

#ifndef _MEMLAYOUT_H_
#define _MEMLAYOUT_H_

#define MEM_RW 0
#define MEM_RO 1
#define MEM_NO 2

#ifndef USER_PAGE_SIZE
#define USER_PAGE_SIZE 4096
#endif

extern const unsigned int PAGE_SIZE;

struct memregion {
    void *from;
    void *to;
    unsigned char mode; // MEM_RW, or MEM_RO, or MEM_NO
};

int get_mem_layout (struct memregion *regions, unsigned int size);

void print_mem_layout(struct memregion *regions, unsigned int size);

#endif //_MEMLAYOUT_H_
