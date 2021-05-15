//
// Created by Michael Kwok on 4/13/21.
//

#ifndef CMPUT379_QUEUE_H
#define CMPUT379_QUEUE_H

#include <stdbool.h>

typedef struct {
    size_t capacity;
    size_t size;
    size_t start;
    size_t end;
    long *array;
} queue_t;

queue_t *queue_new(size_t capacity);

bool queue_full(queue_t *queue);

long queue_push(queue_t *queue, long item);

long queue_pop(queue_t *queue);

#endif //CMPUT379_QUEUE_H
