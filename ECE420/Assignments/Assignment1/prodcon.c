#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define BSIZE 3
#define NUMITEMS 6

typedef struct {
    char buf[BSIZE];
    int occupied;
    int nextin, nextout;
    pthread_mutex_t mutex;
    pthread_cond_t more;
    pthread_cond_t less;
} buffer_t;

buffer_t buffer;

void *producer(void *);
void *consumer(void *);

#define NUM_THREADS 2

pthread_t tid[NUM_THREADS];

int main() {
    int i;
    pthread_cond_init(&(buffer.more), NULL);
    pthread_cond_init(&(buffer.less), NULL);

    pthread_mutex_init(&buffer.mutex, NULL);

    pthread_create(&tid[1], NULL, consumer, NULL);
    pthread_create(&tid[0], NULL, producer, NULL);

    for (i = 0; i < NUM_THREADS; i++) { pthread_join(tid[i], NULL); }

    printf("\nmain() reporting that all %d threads have terminated\n", i);

}

void *producer(void *params) {
    char item[NUMITEMS] = "HELLO.";

    printf("producer started.\n");

    for (int i = 0; i < NUMITEMS; i++) { /* produce an item, one character from item[] */
        if (item[i] == '\0') break;      /* Quit if at end of string. */
        pthread_mutex_lock(&(buffer.mutex));

        if (buffer.occupied >= BSIZE) {
            printf("producer waiting.\n");
        }

        while (buffer.occupied >= BSIZE) {
            pthread_cond_wait(&(buffer.less), &(buffer.mutex));
        }

        printf("producer executing.\n");
        buffer.buf[buffer.nextin] = item[i];
        buffer.nextin++;
        buffer.nextin %= BSIZE;
        buffer.occupied++;
        pthread_cond_signal(&(buffer.more));
        pthread_mutex_unlock(&(buffer.mutex));
    }
    printf("producer exiting.\n");
    pthread_exit(0);
}

void *consumer(void *parm) {
    char *item = malloc(sizeof(char) * (NUMITEMS + 1));
    int i;
    printf("consumer started.\n");
    for (i = 0; i < NUMITEMS; i++) {
        pthread_mutex_lock(&(buffer.mutex));
        while (buffer.occupied <= 0) {
            pthread_cond_wait(&(buffer.more), &(buffer.mutex));
        }
        printf("Consumer executing.\n");
        item[i] = buffer.buf[buffer.nextout];
        buffer.nextout = (buffer.nextout + 1) % BSIZE;
        buffer.occupied--;
        if (buffer.occupied <= 0) {
            printf("consumer waiting.\n");
            pthread_cond_signal(&(buffer.less));
        }
        pthread_mutex_unlock(&(buffer.mutex));

    }
    printf("Received message: %s\n", item);
    printf("consumer exiting.\n");
    pthread_exit(0);
}