//
// Created by Michael Kwok on 4/13/21.
//

#include <malloc.h>
#include <stdbool.h>

#include "queue.h"

queue_t *queue_new(size_t capacity) {
    queue_t *queue = malloc(sizeof(queue_t));

    queue->capacity = capacity;
    queue->start = 0;
    queue->size = 0;
    queue->end = capacity - 1;

    queue->array = malloc(queue->capacity * sizeof(long));

    return queue;
}

bool queue_full(queue_t *queue) {
    return queue->size == queue->capacity;
}

long queue_push(queue_t *queue, long item) {
    long return_val = -1;

    if (queue_full(queue)) {
        return_val = queue_pop(queue);
    }

    queue->end = (queue->end + 1) % queue->capacity;
    queue->array[queue->end] = item;
    queue->size++;

    return return_val;
}

long queue_pop(queue_t *queue) {
    long item = queue->array[queue->start];

    queue->start = (queue->start + 1)
                   % queue->capacity;
    queue->size = queue->size - 1;
    return item;
}
