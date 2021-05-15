//
// Created by Michael Kwok on 4/13/21.
//

#ifndef CMPUT379_LRU_H
#define CMPUT379_LRU_H

#include <stdlib.h>

#include "queue.h"
#include "multiset.h"

typedef struct {
    queue_t *queue;
    set_node_t *multiset;
} lru_t;

lru_t *lru_new(size_t capacity);

size_t get_pages(lru_t *cache);

void visit(lru_t *cache, long page);

#endif //CMPUT379_LRU_H
