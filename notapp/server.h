//
// Created by Michael Kwok on 2/17/21.
//

#ifndef NOTAPP_SERVER_H
#define NOTAPP_SERVER_H

#include <stddef.h>

struct serverinfo {
  char *server_port;
  char *log_file;

  int update_interval;
};

void server(struct serverinfo info);

#endif  // NOTAPP_OBSERVER_H
