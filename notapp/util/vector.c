//
// Created by Michael Kwok on 3/9/21.
//
#include "vector.h"

#include <stdlib.h>

void vector_new(struct vector *this) {
  this->capacity = VECTOR_DEFAULT_CAPACITY;
  this->count = 0;
  this->array = malloc(sizeof(void *) * this->capacity);
  this->mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(this->mutex, NULL);
}

size_t vector_size(struct vector *this) {
  pthread_mutex_lock(this->mutex);
  size_t count = this->count;
  pthread_mutex_unlock(this->mutex);
  return count;
}

void vector_resize(struct vector *this, int new_capacity) {
  pthread_mutex_lock(this->mutex);

  void **temp_array = realloc(this->array, sizeof(void *) * new_capacity);

  if (temp_array != NULL) {
    this->array = temp_array;
    this->capacity = new_capacity;
  }

  pthread_mutex_unlock(this->mutex);
}

void vector_push(struct vector *this, void *item) {
  if (this->count == this->capacity) {
    vector_resize(this, this->capacity << 1);
  }

  pthread_mutex_lock(this->mutex);
  this->array[this->count] = item;
  this->count += 1;
  pthread_mutex_unlock(this->mutex);
}

void *vector_get(struct vector *this, size_t index) {
  if (index >= 0 && index < this->count) {
    pthread_mutex_lock(this->mutex);
    void *item = this->array[index];
    pthread_mutex_unlock(this->mutex);

    return item;
  }
  return NULL;
}

void vector_delete(struct vector *this, size_t index) {
  if (index >= 0 && index < this->count) {
    pthread_mutex_lock(this->mutex);
    this->array[index] = NULL;

    for (int i = index; i < this->count; i++) {
      this->array[i] = this->array[i + 1];
    }

    this->count -= 1;
    this->array[this->count] = NULL;
    pthread_mutex_unlock(this->mutex);

    if (this->capacity > 0 && this->capacity == (this->count >> 2)) {
      vector_resize(this, this->capacity >> 1);
    }
  }
}

void *vector_pop(struct vector *this) {
  void *item = vector_get(this, this->count - 1);
  vector_delete(this, this->count - 1);
  return item;
}

void vector_free(struct vector *this) {
  while (vector_size(this) != 0) {
    vector_pop(this);
  }
  pthread_mutex_lock(this->mutex);
  free(this->array);
  pthread_mutex_unlock(this->mutex);
  pthread_mutex_destroy(this->mutex);
  free(this->mutex);
}
