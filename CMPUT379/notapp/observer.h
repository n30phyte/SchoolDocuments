#ifndef NOTAPP_OBSERVER_H
#define NOTAPP_OBSERVER_H

#include <stddef.h>

struct obsinfo {
  size_t obs_id;

  char *server_address;
  char *server_port;
  char *watch_target;

  int socket_fd;
};

void observer(struct obsinfo info);

#endif  // NOTAPP_OBSERVER_H
