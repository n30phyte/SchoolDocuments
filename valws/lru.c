//
// Created by Michael Kwok on 4/13/21.
//

#include "lru.h"

lru_t *lru_new(size_t capacity) {
    lru_t *cache = malloc(sizeof(lru_t));

    cache->queue = queue_new(capacity);
    cache->multiset = NULL;
    return cache;
}

size_t get_pages(lru_t *cache) {
    return node_count(cache->multiset);
}

void visit(lru_t *cache, long page) {
    cache->multiset = node_insert(cache->multiset, page);
    long val = queue_push(cache->queue, page);
    if (val != -1) {
        cache->multiset = node_delete(cache->multiset, val);
    }
}