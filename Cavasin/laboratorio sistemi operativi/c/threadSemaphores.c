#include <stdio.h>
#include <stdlib.h>
#include "pthread.h"

typedef struct {
  pthread_cond_t cond;
  pthread_mutex_t mutex;
} Synchronize;


_Bool running= 1;



int main(int argv, char** argc) {
  pthread_t producerThread, consumerThread;
  Synchronize synchronize= {PTHREAD_COND_INITIALIZER, PTHREAD_MUTEX_INITIALIZER};
  int threadStatus;

  threadStatus= pthread_create(&producerThread, NULL, processor, &{1, 1, &synchronize});
  printf("threadStatus=%d\n", threadStatus);
  threadStatus= pthread_create(&consumerThread, NULL, processor, &(SynchronizedJob){.job= 1, .synchronize= &synchronize});
  printf("threadStatus=%d\n", threadStatus);

  getchar();
  running= 0;

  return 0;
}