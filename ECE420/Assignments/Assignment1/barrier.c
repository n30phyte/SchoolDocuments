#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    pthread_mutex_t count_lock;
    pthread_cond_t ok_to_proceed;
    int count;
} mylib_barrier_t;

void mylib_init_barrier(mylib_barrier_t *b) {
    b->count = 0;
    pthread_mutex_init(&(b->count_lock), NULL);
    pthread_cond_init(&(b->ok_to_proceed), NULL);
}

void mylib_barrier(mylib_barrier_t *b, int num_threads) {
    pthread_mutex_lock(&(b->count_lock));
    (b->count)++;

    if (b->count == num_threads) {
        b->count = 0;
        pthread_cond_broadcast(&b->ok_to_proceed);
    } else {
        while (pthread_cond_wait(&b->ok_to_proceed, &(b->count_lock)) != 0);
    }

    pthread_mutex_unlock(&(b->count_lock));
}

int main() {
    mylib_barrier_t *barrier = malloc(sizeof(mylib_barrier_t));
    mylib_init_barrier(barrier);
    free(barrier);
    return 0;
}
