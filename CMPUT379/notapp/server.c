//
// Created by Michael Kwok on 2/17/21.
//

#include "server.h"

#include <errno.h>
#include <memory.h>
#include <netdb.h>
#include <pthread.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#include "util/hashtable.h"
#include "util/logger.h"
#include "util/util.h"
#include "util/vector.h"

struct watch_info {
  int socket_fd;
  int update_interval;
};

struct obs_info {
  int socket_fd;
  size_t thread_id;

  char *watch_target;
  char *host_addr;
};

static struct hash_table *update_list;
static struct logger *logger;
static char *server_port;

static sigjmp_buf server_exit;
static volatile bool server_running = true;

void signal_handler(int sig_num) {
  if (sig_num != SIGPIPE) {
    logger_write(logger, "Signal handler called. Terminating program.");
    server_running = false;
    siglongjmp(server_exit, 1);
  }
}

void server_daemonize() {
  switch (fork()) {
    case 0:
      break;
    case -1:
      fprintf(stderr, "Failed to demonize\n");
      _exit(EXIT_FAILURE);
    default:
      _exit(EXIT_SUCCESS);
  }

  if (setsid() == -1) {
    fprintf(stderr, "Failed to demonize\n");
    _exit(EXIT_FAILURE);
  }
  switch (fork()) {
    case 0:
      break;
    case -1:
      fprintf(stderr, "Failed to demonize\n");
      _exit(EXIT_FAILURE);
    default:
      _exit(EXIT_SUCCESS);
  }

  fprintf(stderr, "%s\n", server_port);

  close(STDIN_FILENO);
}

void *observer_handler(void *args) {
  bool observer_running = true;
  struct obs_info arguments;

  memcpy(&arguments, args, sizeof(struct obs_info));
  free(args);

  char *buffer = malloc(255 * sizeof(char));
  sprintf(buffer, "A");
  send_msg(arguments.socket_fd, buffer);

  while (observer_running && server_running) {
    if (recv_msg(arguments.socket_fd, buffer) > 0) {
      struct vector *message = split_string(buffer);

      char command = *(char *)vector_get(message, 0);

      struct update_msg *new_update = malloc(sizeof(struct update_msg));

      switch (command) {
        case 'D':
          new_update->is_delete = true;
          hash_set(update_list, arguments.watch_target, new_update);

          send_msg(arguments.socket_fd, "A");
          logger_write(logger, "Observer %d disconnected.",
                       arguments.thread_id);
          observer_running = false;
          free(new_update);
          break;
        case 'M':
          new_update->is_delete = false;
          new_update->inotify_mask = strtol(vector_get(message, 2), NULL, 10);
          char *time_us;
#ifdef NOTAPP_TIME
          struct timeval tv;
          gettimeofday(&tv, NULL);

          new_update->time_s = tv.tv_sec;
          new_update->time_us = tv.tv_usec;
#else
          new_update->time_s = strtol(vector_get(message, 1), &time_us, 10);
          new_update->time_us = strtol(time_us + 1, NULL, 10);
#endif

          char *file = vector_get(message, 3);
          copy_string(&new_update->changed_file, file);
          copy_string(&new_update->host, arguments.host_addr);
          hash_set(update_list, arguments.watch_target, new_update);
          break;
        default:
          continue;
      }
      vector_free(message);
    } else {
      observer_running = false;
    }
  }

  if (observer_running && !server_running) {
    send_msg(arguments.socket_fd, "D");
    hash_delete(update_list, arguments.watch_target);
  }

  free(buffer);

  pthread_exit(NULL);
}

void send_update(int fd, char *key, struct update_msg *update) {
  char *buffer = malloc(512 * sizeof(char));

  if (update->is_delete) {
    sprintf(buffer, "D %s", key);
    hash_delete(update_list, key);
  } else {
    sprintf(buffer, "M %ld.%ld %s %s %u %s", update->time_s, update->time_us,
            update->host, key, update->inotify_mask, update->changed_file);
  }

  send_msg(fd, buffer);
  free(buffer);
}

void *watch_handler(void *args) {
  bool watch_running = true;
  struct watch_info arguments;
  memcpy(&arguments, args, sizeof(struct watch_info));
  free(args);

  struct timeval last_update;
  gettimeofday(&last_update, NULL);

  send_msg(arguments.socket_fd, "A");

  char *buffer = malloc(255 * sizeof(char));

  while (watch_running && server_running) {
    int messages;
    ioctl(arguments.socket_fd, FIONREAD, &messages);

    if (messages != 0) {
      recv_msg(arguments.socket_fd, buffer);
      watch_running = false;
      send_msg(arguments.socket_fd, "A");
      logger_write(logger, "Client disconnected.");
    } else {
      struct timeval current_time;
      gettimeofday(&current_time, NULL);

      struct timeval comptime;
      timersub(&current_time, &last_update, &comptime);

      struct timeval interval = {.tv_usec = arguments.update_interval * 1000};

      if (timercmp(&comptime, &interval, >=)) {
        // Update client
        struct hash_iterator iterator = {.hashtable = update_list,
                                         .index = 0,
                                         .item = update_list->table[0]};

        struct hash_entry *item = hash_iterate(&iterator);
        while (item != NULL) {
          struct update_msg *to_send = (struct update_msg *)item->value;
          struct timeval update_time = {
              .tv_sec = to_send->time_s,
              .tv_usec = to_send->time_us,
          };

          // Only send update if the update came after send.
          if (timercmp(&update_time, &last_update, >=)) {
            send_update(arguments.socket_fd, item->key, to_send);
          }

          item = hash_iterate(&iterator);
        }
        last_update = current_time;
      }
    }
  }

  free(buffer);
  pthread_exit(NULL);
}

void server(struct serverinfo info) {
  server_port = info.server_port;
  server_daemonize();
  logger = logger_init(info.log_file);
  logger_write(logger, "Starting server, listening to port %s.",
               info.server_port);

  struct sigaction act = {.sa_handler = signal_handler,
                          .sa_flags = SA_RESETHAND};

  sigemptyset(&act.sa_mask);
  sigaction(SIGINT, &act, NULL);
  sigaction(SIGTERM, &act, NULL);
  sigaction(SIGPIPE, &act, NULL);

  struct addrinfo hints, *res;

  memset(&hints, 0, sizeof(hints));

  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;

  struct vector obs_threads;
  struct vector watch_threads;
  update_list = malloc(sizeof(struct hash_table));

  vector_new(&obs_threads);
  vector_new(&watch_threads);
  hash_new(update_list);

  int status = getaddrinfo(NULL, info.server_port, &hints, &res);
  if (status != 0) {
    logger_write(logger, "getaddrinfo error: %s.", gai_strerror(status));
    server_running = false;
    goto cleanup;
  }

  // Create new socket
  int socket_fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

  if (socket_fd < 0) {
    logger_write(logger, "Error creating socket: %d.", errno);
    server_running = false;
    goto cleanup;
  }

  status = bind(socket_fd, res->ai_addr, res->ai_addrlen);
  if (status < 0) {
    logger_write(logger, "Error creating socket: %d.", errno);
    server_running = false;
    goto cleanup;
  }

  status = listen(socket_fd, 5);
  if (status < 0) {
    logger_write(logger, "Listen failed, %d.", errno);
    server_running = false;
    goto cleanup;
  }

  sigsetjmp(server_exit, true);
  while (server_running) {
    struct sockaddr_storage client_addr;
    socklen_t client_addr_size = sizeof(client_addr);

    // Wait for new connections
    int new_socket =
        accept(socket_fd, (struct sockaddr *)&client_addr, &client_addr_size);
    if (new_socket < 0) {
      logger_write(logger, "Accept failed, %d.", errno);
      continue;
    }

    // Check if observer or client
    char *buffer = malloc(255 * sizeof(char));
    recv_msg(new_socket, buffer);
    struct vector *commands = split_string(buffer);
    char *client_type = vector_get(commands, 1);

    pthread_t tid;

    if (*client_type == 'O') {
      logger_write(logger, "Adding new observer.");

      struct obs_info *args = malloc(sizeof(struct obs_info));

      char *watch_target = vector_get(commands, 2);

      copy_string(&args->watch_target, watch_target);

      args->host_addr = malloc(INET_ADDRSTRLEN * sizeof(char));
      args->thread_id = vector_size(&obs_threads);
      args->socket_fd = new_socket;
      getnameinfo((struct sockaddr *)&client_addr, client_addr_size,
                  args->host_addr, INET_ADDRSTRLEN, NULL, 0, NI_NUMERICHOST);

      pthread_create(&tid, NULL, observer_handler, (void *)args);
      vector_push(&obs_threads, (void *)tid);
    } else {
      logger_write(logger, "Adding new client.");
      struct watch_info *args = malloc(sizeof(struct obs_info));
      args->socket_fd = new_socket;
      args->update_interval = info.update_interval;

      pthread_create(&tid, NULL, watch_handler, (void *)args);
      vector_push(&watch_threads, (void *)tid);
    }

    free(buffer);
    vector_free(commands);
  }

  logger_write(logger, "Cleaning up threads.");
  while (vector_size(&obs_threads) != 0) {
    pthread_t thread_id = (pthread_t)vector_pop(&obs_threads);
    pthread_join(thread_id, NULL);
  }

  while (vector_size(&watch_threads) != 0) {
    pthread_t thread_id = (pthread_t)vector_pop(&watch_threads);
    pthread_join(thread_id, NULL);
  }

cleanup:
  logger_close(logger);
  freeaddrinfo(res);
  hash_free(update_list);
  vector_free(&obs_threads);
  vector_free(&watch_threads);
}
