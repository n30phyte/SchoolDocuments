#include "observer.h"

#include <errno.h>
#include <memory.h>
#include <netdb.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#include "util/util.h"
#include "util/vector.h"

static bool obs_running = true;
static bool sigpipe = false;

static sigjmp_buf obs_exit;

static void obs_sigint_handler(int sig_num) {
  write(STDERR_FILENO, "Signal handler called. Terminating program\n", 42);
  obs_running = false;
  if (sig_num == SIGPIPE) {
    sigpipe = true;
  }
  siglongjmp(obs_exit, 1);
}

void handle_inotify_evt(struct obsinfo *info, struct inotify_event *event) {
  // https://stackoverflow.com/questions/5833094/get-a-timestamp-in-c-in-microseconds
  struct timeval tv;
  gettimeofday(&tv, NULL);

  if (event->mask & (IN_DELETE_SELF | IN_IGNORED)) {
    fprintf(stderr, "Watch deleted.\n");
    obs_running = false;
    siglongjmp(obs_exit, 1);
  } else {
    // Send update_msg to server
    char *buffer = malloc(255 * sizeof(char));
    if (event->len == 0) {
      sprintf(buffer, "M %ld.%ld %u (null)", tv.tv_sec, tv.tv_usec,
              event->mask);
    } else {
      sprintf(buffer, "M %ld.%ld %u %s", tv.tv_sec, tv.tv_usec, event->mask,
              event->name);
    }

    send_msg(info->socket_fd, buffer);
    free(buffer);
  }
}

void observer_handshake_server(struct obsinfo *info) {
  struct addrinfo hints, *res;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;

  int val = getaddrinfo(info->server_address, info->server_port, &hints, &res);
  int counter = 0;

  while (val != 0 && counter <= 3) {
    fprintf(stderr, "getaddrinfo. Trying again in a second...\n");
    sleep(1);
    val = getaddrinfo(info->server_address, info->server_port, &hints, &res);
    if (counter == 3) {
      fprintf(stderr, "getaddrinfo failed with error %d\n", val);
    }
    counter++;
  }
  // Create new socket
  info->socket_fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

  if (info->socket_fd < 0) {
    fprintf(stderr, "Error creating socket: %d\n", errno);
    freeaddrinfo(res);
    exit(1);
  }

  if (connect(info->socket_fd, res->ai_addr, res->ai_addrlen) != 0) {
    fprintf(stderr, "Error connecting to server\n");
    freeaddrinfo(res);
    exit(1);
  }

  char *buffer = malloc(255 * sizeof(char));

  // Send handshake to server with file name
  sprintf(buffer, "S O %s", info->watch_target);
  send_msg(info->socket_fd, buffer);
  free(buffer);

  // Wait for server to send back ID, return received ID
  buffer = malloc(3 * sizeof(char));
  recv_msg(info->socket_fd, buffer);

  if (*buffer == 'D') {
    obs_running = false;
  }

  free(buffer);
  freeaddrinfo(res);
}

// This call will block until the server responds
void observer_disconnect_server(struct obsinfo *info) {
  // Tell server client disconnecting
  send_msg(info->socket_fd, "D");

  char *buffer = malloc(255 * sizeof(char));
  // Wait for server to send back ACK
  recv_msg(info->socket_fd, buffer);

  free(buffer);
}

void observer(struct obsinfo info) {
  fprintf(stderr,
          "Starting observer, connecting to server %s: %s, watching %s.\n",
          info.server_address, info.server_port, info.watch_target);

  observer_handshake_server(&info);
  if (!obs_running) {
    fprintf(stderr, "Server connection error!\n");
    exit(EXIT_FAILURE);
  }

  fprintf(stderr, "Hooking into sigint handler\n");
  struct sigaction act = {.sa_handler = obs_sigint_handler,
                          .sa_flags = SA_RESETHAND};

  sigemptyset(&act.sa_mask);
  sigaction(SIGINT, &act, NULL);
  sigaction(SIGPIPE, &act, NULL);

  fprintf(stderr, "Adding inotify listener\n");

  int inotify_fd = inotify_init1(IN_CLOEXEC);
  if (inotify_fd < 0) {
    fprintf(stderr, "Inotify init for file: %s had error: %d!\n",
            info.watch_target, inotify_fd);
    exit(EXIT_FAILURE);
  }

  int inotify_wd =
      inotify_add_watch(inotify_fd, info.watch_target, IN_ALL_EVENTS);
  if (inotify_wd < 0) {
    fprintf(stderr, "Inotify watch for file: %s had error: %d!\n",
            info.watch_target, inotify_wd);
    exit(EXIT_FAILURE);
  }

  sigsetjmp(obs_exit, true);
  while (obs_running) {
    unsigned int available;
    ioctl(inotify_fd, FIONREAD, &available);

    char *buffer = malloc(available * sizeof(char));
    read(inotify_fd, buffer, available);

    size_t offset = 0;

    while (offset < available) {
      struct inotify_event *new_event =
          (struct inotify_event *)(buffer + offset);
      handle_inotify_evt(&info, new_event);

      offset += sizeof(struct inotify_event) + new_event->len;
    }
    free(buffer);
  }

  if (!sigpipe) {
    observer_disconnect_server(&info);
  }

  exit(EXIT_SUCCESS);
}
