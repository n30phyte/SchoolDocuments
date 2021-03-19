#ifndef NOTAPP_UTIL_H
#define NOTAPP_UTIL_H

#include <stdbool.h>
#include <stdint.h>

struct update_msg {
  long int time_s;
  long int time_us;

  uint32_t inotify_mask;

  char *host;
  char *changed_file;

  bool is_delete;
};

void send_msg(int socket_fd, const char *message);

int recv_msg(int socket_fd, char *message);

struct vector *split_string(const char *text);

void copy_string(char **dest, const char *source);

#endif  // NOTAPP_UTIL_H
