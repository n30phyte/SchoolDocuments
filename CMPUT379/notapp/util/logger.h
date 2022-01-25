#ifndef NOTAPP_LOGGER_H
#define NOTAPP_LOGGER_H

#include <pthread.h>
#include <stdio.h>

struct logger {
  FILE *file;
  pthread_mutex_t *mutex;
  short counter;
};

struct logger *logger_init(char *filename);

void logger_write(struct logger *logger, char *format_str, ...);

void logger_close(struct logger *logger);

#endif  // NOTAPP_LOGGER_H
