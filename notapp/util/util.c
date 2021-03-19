#include "util.h"

#include <netdb.h>
#include <stdlib.h>
#include <string.h>

#include "vector.h"

/**
 * Send a message through the specified socket, adding a \n at the end to be
 * used as an end delimiter.
 *
 * @param socket_fd Socket to send to.
 * @param message Message to send.
 */
void send_msg(const int socket_fd, const char *message) {
  const unsigned long orig_len = strlen(message);
  const unsigned long message_size = orig_len + 1;

  char *msg_newline = malloc((message_size + 1) * sizeof(char));
  char *msg_ptr = msg_newline;

  strncpy(msg_newline, message, orig_len);
  msg_newline[orig_len] = '\n';
  msg_newline[orig_len + 1] = '\0';

  int sent_size = send(socket_fd, msg_ptr, message_size, 0);
  msg_ptr += sent_size;
  while (sent_size > 0 && msg_ptr < (msg_newline + message_size)) {
    sent_size = send(socket_fd, msg_ptr, message_size - sent_size, 0);
    msg_ptr += sent_size;
  }

  free(msg_newline);
}

/**
 * Receive a message over the socket that has a \n as the delimiter.
 * Buffer should have enough storage for the incoming message.
 *
 * @param socket_fd Socket to send to.
 * @param message Pointer to buffer.
 */
int recv_msg(const int socket_fd, char *message) {
  // Check if anything is available
  int current_length = 0;
  char *msg_ptr = message;

  int received_size = recv(socket_fd, msg_ptr, 1, 0);

  while (*msg_ptr != '\n' && received_size > 0) {
    msg_ptr += 1;
    current_length += 1;
    received_size = recv(socket_fd, msg_ptr, 1, 0);
  }

  message[current_length] = '\0';

  return received_size;
}

/**
 * Split a string by space into a vector of strings.
 *
 * @param text String to split.
 */
struct vector *split_string(const char *text) {
  char *process;
  copy_string(&process, text);
  char *saveptr;
  struct vector *output = malloc(sizeof(struct vector));
  vector_new(output);
  char *str_start = strtok_r(process, " ", &saveptr);

  while (str_start && str_start != NULL) {
    vector_push(output, str_start);
    str_start = strtok_r(NULL, " ", &saveptr);
  }

  return output;
}

/**
 * Like asprintf but for strcpy.
 *
 * @param dest Address of pointer to copy the text to.
 * @param source String to copy.
 */
void copy_string(char **dest, const char *source) {
  if (source == NULL) {
    *dest = NULL;
  } else {
    size_t str_len = strlen(source);
    *dest = malloc(sizeof(char) * (str_len + 1));
    strncpy(*dest, source, str_len);
    (*dest)[str_len] = '\0';
  }
}
