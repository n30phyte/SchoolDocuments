#include "hashtable.h"

#include <stdlib.h>
#include <string.h>

#include "util.h"

// 32-bit FNV-1a hash function from Wikipedia.
// Good enough for this assignment.
size_t fnv(char *key) {
  size_t hash = 0x01000193;
  const size_t fnv_prime = 0x01000193;

  while (*key) {
    hash = hash ^ (*key);
    hash *= fnv_prime;
    key++;
  }

  return hash;
}

/**
 * Create a new hash table.
 *
 * @param this Hash table struct that this is initializing.
 */
void hash_new(struct hash_table *this) {
  this->capacity = HASH_DEFAULT_CAPACITY;
  this->count = 0;
  this->table = malloc(this->capacity * sizeof(struct hash_entry));
  for (size_t i = 0; i < this->capacity; i++) {
    this->table[i] = NULL;
  }
  this->mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(this->mutex, NULL);
}

/**
 * Insert a value into the hash table.
 *
 * @param this Hash table struct that this will be inserting to.
 * @param key The key of the value.
 * @param value Value to be inserted.
 */
void hash_set(struct hash_table *this, char *key, void *value) {
  size_t idx = fnv(key) % this->capacity;

  pthread_mutex_lock(this->mutex);

  struct hash_entry *existing = this->table[idx];

  // Loop until it finds the right place.
  while (existing != NULL) {
    if (!strcmp(existing->key, key)) {
      existing->value = value;
      pthread_mutex_unlock(this->mutex);
      return;
    }
    existing = existing->next;
  }

  // Allocate space for new item.
  existing = malloc(sizeof(struct hash_entry));
  copy_string(&existing->key, key);
  existing->value = value;
  existing->next = this->table[idx];
  this->table[idx] = existing;
  this->count++;
  pthread_mutex_unlock(this->mutex);
}

/**
 * Delete specified key from this table
 *
 * @param this hash table struct to delete from.
 * @param key Key to delete.
 */
void hash_delete(struct hash_table *this, char *key) {
  size_t idx = fnv(key) % this->capacity;

  pthread_mutex_lock(this->mutex);
  struct hash_entry *existing = this->table[idx];
  struct hash_entry *prev = NULL;

  while (existing != NULL) {
    if (!strcmp(key, existing->key)) {
      free(existing->value);
      if (prev != NULL) {
        prev->next = existing->next;
      } else {
        this->table[idx] = existing->next;
      }
      free(existing);
      this->count--;
      break;
    }
    prev = existing;
    existing = existing->next;
  }
  pthread_mutex_unlock(this->mutex);
}

/**
 * A function to help iterate through existing hash table entries.
 *
 * @param iterator Iterator struct to use to keep track of the iterations.
 * @return hash_entry structs from the hash table.
 */
struct hash_entry *hash_iterate(struct hash_iterator *iterator) {
  while (iterator->item == NULL) {
    if (iterator->index < iterator->hashtable->capacity - 1) {
      iterator->index++;
      iterator->item = iterator->hashtable->table[iterator->index];
    } else {
      return NULL;
    }
  }

  struct hash_entry *item = iterator->item;

  if (item) {
    iterator->item = item->next;
  }
  return item;
}

/**
 * Clean up the hashtable.
 *
 * @param this Hashtable to clean up.
 */
void hash_free(struct hash_table *this) {
  pthread_mutex_lock(this->mutex);
  free(this->table);
  pthread_mutex_unlock(this->mutex);
  pthread_mutex_destroy(this->mutex);
  free(this->mutex);
}
