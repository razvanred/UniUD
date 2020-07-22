#include <stdio.h>
#include <stdlib.h>
#include "pthread.h"

typedef struct {
  pthread_cond_t cond;
  pthread_mutex_t mutex;
} Synchronize;

#define THREADARGS(...) \
  {                     \
    struct ThreadArgs { \
      __VA_ARGS__       \
    };                  \
  }

_Bool running= 1;

void* processor(struct {
  unsigned int threadId;
  unsigned int jobId;
  Synchronize* synchronize;
}; * args) {
  pthread_mutex_lock(&args->synchronize->mutex);
  pthread_cond_wait(&args->synchronize->cond, &args->synchronize->mutex);

  printf("thread %d processing job %d\n", args->threadId, args->jobId);

  pthread_mutex_unlock(&args->synchronize->mutex);
}

int main(int argv, char** argc) {
  pthread_t producerThread, consumerThread;
  Synchronize synchronize= {PTHREAD_COND_INITIALIZER, PTHREAD_MUTEX_INITIALIZER};
  int threadStatus;

  threadStatus= pthread_create(&producerThread, NULL, processor, &{1, 1, &synchronize});
  printf("threadStatus=%d\n", threadStatus);
  threadStatus= pthread_create(&consumerThread, NULL, processor, &(SynchronizedJob){.job= 1, .synchronize= &synchronize});
  printf("threadStatus=%d\n", threadStatus);

  { int a= 5; }

  getchar();
  running= 0;

  return 0;
}