#ifndef NOTAPP_VECTOR_HPP
#define NOTAPP_VECTOR_HPP

#include <pthread.h>

#define VECTOR_DEFAULT_CAPACITY 10

struct vector {
  void **array;
  size_t count;
  size_t capacity;

  pthread_mutex_t *mutex;
};

void vector_new(struct vector *this);

void vector_resize(struct vector *this, int new_capacity);

void vector_push(struct vector *this, void *item);

void *vector_get(struct vector *this, size_t index);

void vector_set(struct vector *this, size_t index, void *item);

void vector_delete(struct vector *this, size_t index);

void *vector_pop(struct vector *this);

size_t vector_size(struct vector *this);

size_t vector_capacity(struct vector *this);

void vector_free(struct vector *this);

#endif  // NOTAPP_VECTOR_HPP
