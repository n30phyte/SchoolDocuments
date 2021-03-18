#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "client.h"
#include "observer.h"
#include "server.h"
#include "util/util.h"

int main(int argc, char *argv[]) {
  bool server_flag = false;
  bool observer_flag = false;
  bool client_flag = false;

  int interval_ms = -1;
  float interval_s;

  char *fname = NULL;
  char *server_addr = NULL;
  char *server_port = NULL;
  int opt;

  char *shortopts = "sout:p:l:";

  while ((opt = getopt(argc, argv, shortopts)) != -1) {
    switch (opt) {
      case 's':
        server_flag = true;
        break;
      case 'o':
        observer_flag = true;

        copy_string(&server_addr, argv[optind]);
        copy_string(&server_port, argv[optind + 1]);
        copy_string(&fname, argv[optind + 2]);
        break;
      case 'u':
        client_flag = true;

        copy_string(&server_addr, argv[optind]);
        copy_string(&server_port, argv[optind + 1]);
        break;
      case 't':
        interval_s = strtof(optarg, NULL);
        interval_ms = (int)(interval_s * 1000);
        break;
      case 'p':
        copy_string(&server_port, optarg);
        break;
      case 'l':
        copy_string(&fname, optarg);
      default:
        break;
    }
  }

  // Ensure only one flag is set
  if ((server_flag ^ observer_flag ^ client_flag) != 1) {
    printf("Incorrect usage. Arguments:\n");
    printf(
        "  Server:\n    notapp -s -t <interval> [-p <sport>] [-l "
        "<log_file>]\n");
    printf("  Observer:\n    notapp -o <saddr> <sport> <fileordir>\n");
    printf("  Client:\n    notapp -u <saddr> <sport>\n");
  } else {
    if (server_flag) {
      struct serverinfo info = {.update_interval = interval_ms};
      copy_string(&info.log_file, fname);
      copy_string(&info.server_port, server_port);

      server(info);
    } else if (observer_flag) {
      struct obsinfo info = {
          .obs_id = 0,
          .socket_fd = -1,
      };

      copy_string(&info.server_address, server_addr);
      copy_string(&info.server_port, server_port);
      copy_string(&info.watch_target, fname);

      observer(info);
    } else {
      client(server_addr, server_port);
    }
  }

  free(server_addr);
  free(server_port);
  free(fname);

  return 0;
}
