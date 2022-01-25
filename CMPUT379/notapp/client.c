#include "client.h"

#include <errno.h>
#include <memory.h>
#include <ncurses.h>
#include <netdb.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <unistd.h>

#include "util/hashtable.h"
#include "util/util.h"
#include "util/vector.h"

static bool client_running = true;
static sigjmp_buf client_exit;
static int socket_fd;

/**
 * Sending disconnect message to server
 */
void client_disconnect_server() {
  // Tell server disconnecting
  send_msg(socket_fd, "D");

  char *buffer = malloc(10 * sizeof(char));
  // Wait for server to send back ACK
  recv_msg(socket_fd, buffer);

  free(buffer);
}

static void client_sigint_handler(int sig_num) {
  write(STDOUT_FILENO, "Signal handler called. Terminating program.\n", 42);
  client_running = false;
  if (sig_num != SIGPIPE) {
    client_disconnect_server();
  }
  endwin();

  exit(EXIT_SUCCESS);
}

void process_inotify(char **outstr, uint32_t inotify_flags, char *filename) {
  char *out;

  if (strcmp(filename, "(null)") != 0) {
    out = malloc(strlen(filename));
    out = strcpy(out, filename);
  } else {
    out = malloc(2);
    out[0] = '\0';
  }

  if (inotify_flags & IN_ACCESS) {
    char text[] = " IN_ACCESS";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_ATTRIB) {
    char text[] = " IN_ATTRIB";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_CLOSE_NOWRITE) {
    char text[] = " IN_CLOSE_NOWRITE";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_CLOSE_WRITE) {
    char text[] = " IN_CLOSE_WRITE";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_CREATE) {
    char text[] = " IN_CREATE";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_DELETE) {
    char text[] = " IN_DELETE";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_DELETE_SELF) {
    char text[] = " IN_DELETE_SELF";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_IGNORED) {
    char text[] = " IN_IGNORED";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_ISDIR) {
    char text[] = " IN_ISDIR";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_MODIFY) {
    char text[] = " IN_MODIFY";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_MOVE_SELF) {
    char text[] = " IN_MOVE_SELF";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_MOVED_FROM) {
    char text[] = " IN_MOVED_FROM";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_MOVED_TO) {
    char text[] = " IN_MOVED_TO";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_OPEN) {
    char text[] = " IN_OPEN";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_Q_OVERFLOW) {
    char text[] = " IN_Q_OVERFLOW";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }
  if (inotify_flags & IN_UNMOUNT) {
    char text[] = " IN_UNMOUNT";
    out = realloc(out, strlen(out) + strlen(text) + 1);
    out = strcat(out, text);
  }

  *outstr = out;
}

void client_ui() {
  initscr();
  cbreak();
  struct hash_table printed;

  hash_new(&printed);

  printw("%-20s %-20s %-20s %-80s\n", "TIME", "HOST", "MONITORED", "EVENT");
  refresh();

  sigsetjmp(client_exit, true);
  while (client_running) {
    move(1, 0);
    refresh();
    char *buffer = malloc(255 * sizeof(char));

    // Check available data
    unsigned int available;
    ioctl(socket_fd, FIONREAD, &available);

    // Only do updates when there's new data.
    while (available > 0) {
      recv_msg(socket_fd, buffer);
      struct update_msg *update = malloc(sizeof(struct update_msg));

      struct vector *message = split_string(buffer);

      char command = *(char *)vector_get(message, 0);

      if (command == 'D') {
        hash_delete(&printed, vector_get(message, 1));
        clear();
        printw("%-20s %-20s %-20s %-80s\n", "TIME", "HOST", "MONITORED",
               "EVENT");
        refresh();
      } else {
        char *watch_file;
        char *time_us;
        update->time_s = strtol(vector_get(message, 1), &time_us, 10);
        update->time_us = strtol(time_us + 1, NULL, 10);
        copy_string(&update->host, vector_get(message, 2));
        copy_string(&watch_file, vector_get(message, 3));
        update->inotify_mask = strtol(vector_get(message, 4), NULL, 10);
        copy_string(&update->changed_file, vector_get(message, 5));
        hash_set(&printed, watch_file, update);
      }

      vector_free(message);
      ioctl(socket_fd, FIONREAD, &available);

      struct hash_iterator iterator = {
          .hashtable = &printed, .index = 0, .item = printed.table[0]};
      struct hash_entry *item = hash_iterate(&iterator);

      while (item != NULL) {
        update = (struct update_msg *)item->value;
        sprintf(buffer, "%ld.%ld", update->time_s, update->time_us);

        char *inotify_msg;
        process_inotify(&inotify_msg, update->inotify_mask,
                        update->changed_file);

        printw("%-20s %-20s %-20s %-80s\n", buffer, update->host, item->key,
               inotify_msg);
        free(inotify_msg);
        item = hash_iterate(&iterator);
      }
      refresh();
    }

    free(buffer);
  }

  hash_free(&printed);
  endwin();
}

void client_handshake_server(char *server_address, char *port_number) {
  fprintf(stderr, "Hooking into sigint handler\n");
  struct sigaction act = {.sa_handler = client_sigint_handler,
                          .sa_flags = SA_RESETHAND};

  sigemptyset(&act.sa_mask);
  sigaction(SIGINT, &act, NULL);
  sigaction(SIGPIPE, &act, NULL);

  struct addrinfo hints, *res;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;

  int val = getaddrinfo(server_address, port_number, &hints, &res);
  int counter = 0;

  while (val != 0 && counter <= 3) {
    fprintf(stderr, "getaddrinfo. Trying again in a second...\n");
    sleep(1);
    val = getaddrinfo(server_address, port_number, &hints, &res);
    if (counter == 3) {
      fprintf(stderr, "getaddrinfo failed with error %d.\n", val);
    }
    counter++;
  }

  // Create new socket
  socket_fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

  if (socket_fd < 0) {
    fprintf(stderr, "Error creating socket: %d.\n", errno);
    freeaddrinfo(res);
    exit(1);
  }

  if (connect(socket_fd, res->ai_addr, res->ai_addrlen) != 0) {
    fprintf(stderr, "Error connecting to server: %d.\n", errno);
    freeaddrinfo(res);
    exit(1);
  }

  // Send handshake to server stating it's a watch client.
  send_msg(socket_fd, "S W");

  // Wait for server to send back ACK
  char *buffer = malloc(2 * sizeof(char));
  recv_msg(socket_fd, buffer);

  free(buffer);
  freeaddrinfo(res);
}

void client(char *server_address, char *port_number) {
  fprintf(stderr, "Connecting to server %s:%s.\n", server_address, port_number);

  client_handshake_server(server_address, port_number);
  if (!client_running) {
    fprintf(stderr, "Server connection error!\n");
    exit(EXIT_FAILURE);
  }

  client_ui();
}
