#include "logger.h"

#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

/**
 * Initialize a logger to write to the specified file.
 *
 * @param filename File to write the logging messages into.
 * @return A logger struct that should be used for future logging operations.
 */
struct logger *logger_init(char *filename) {
  struct logger *this = malloc(sizeof(struct logger));
  this->mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(this->mutex, NULL);

  this->file = fopen(filename, "a+");
  this->counter = 0;
  return this;
}

/**
 * A wrapper over vprintf that logs the message into
 *
 * @param logger Logger object to used for writing the messages
 * @param format_str Format string to be passed into vprintf
 * @param ...
 */
void logger_write(struct logger *logger, char *format_str, ...) {
  struct timeval tv;
  gettimeofday(&tv, NULL);

  va_list list;
  va_start(list, format_str);
  pthread_mutex_lock(logger->mutex);
  fprintf(logger->file, "[%lu.%lu] ", tv.tv_sec, tv.tv_usec);
  vfprintf(logger->file, format_str, list);
  fprintf(logger->file, "\n");
  pthread_mutex_unlock(logger->mutex);
  va_end(list);

  if (logger->counter % 5 == 0) {
    fflush(logger->file);
    logger->counter = 0;
  }
}

/**
 * Close the logger
 *
 * @param logger Logger to close
 */
void logger_close(struct logger *logger) {
  pthread_mutex_lock(logger->mutex);
  fflush(logger->file);
  fclose(logger->file);
  pthread_mutex_unlock(logger->mutex);
  pthread_mutex_destroy(logger->mutex);
  free(logger->mutex);
}
