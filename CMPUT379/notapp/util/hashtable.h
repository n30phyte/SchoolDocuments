#ifndef NOTAPP_HASHTABLE_HPP
#define NOTAPP_HASHTABLE_HPP

#include <pthread.h>
#include <stddef.h>

#define HASH_DEFAULT_CAPACITY 20

struct hash_entry {
  char *key;
  void *value;
  struct hash_entry *next;
};

struct hash_table {
  size_t capacity;
  size_t count;
  struct hash_entry **table;
  pthread_mutex_t *mutex;
};

struct hash_iterator {
  struct hash_table *hashtable;
  size_t index;
  struct hash_entry *item;
};

void hash_new(struct hash_table *this);

void hash_set(struct hash_table *this, char *key, void *value);

void hash_delete(struct hash_table *this, char *key);

void hash_free(struct hash_table *this);

struct hash_entry *hash_iterate(struct hash_iterator *iterator);

#endif  // NOTAPP_HASHTABLE_HPP
