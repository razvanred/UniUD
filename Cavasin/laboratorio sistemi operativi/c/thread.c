#include <stdio.h>
#include <stdlib.h>
#include "pthread.h"

typedef struct {
  unsigned char* what;
  unsigned int amount;
} Span;

void* numbers(void* args) {
  Span* span= (Span*)args;
  for(unsigned int i= 0; i < span->amount; i++) {
    printf("%s ", span->what);
  }
  return NULL;
}

int main(int argv, char** argc) {
  pthread_t thread1, thread2;
  int threadStatus;

  threadStatus= pthread_create(&thread1, NULL, numbers, &(Span){.what= "big", .amount= 5000});
  printf("threadStatus=%d\n", threadStatus);
  threadStatus= pthread_create(&thread2, NULL, numbers, &(Span){.what= "pepee", .amount= 5000});
  printf("threadStatus=%d\n", threadStatus);

  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);

  getchar();

  return 0;
}