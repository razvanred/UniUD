#include <stdio.h>
#include <stdlib.h>
#include "pthread.h"

typedef struct {
  unsigned char* string;
  pthread_mutex_t mutex;
} AtomicString;

_Bool running= 1;

void* uppercase(void* args) {
  AtomicString* atomicString= args;
  while(running) {
    pthread_mutex_lock(&atomicString->mutex);
    for(unsigned int i= 0; atomicString->string[i]; i++) {
      if(atomicString->string[i] >= 'a' && atomicString->string[i] <= 'z') {
        atomicString->string[i]-= 32;
      }
    }
    puts(atomicString->string);
    pthread_mutex_unlock(&atomicString->mutex);
  }
}

void* lowercase(void* args) {
  AtomicString* atomicString= args;
  while(running) {
    pthread_mutex_lock(&atomicString->mutex);
    for(unsigned int i= 0; atomicString->string[i]; i++) {
      if(atomicString->string[i] >= 'A' && atomicString->string[i] <= 'Z') {
        atomicString->string[i]+= 32;
      }
    }
    puts(atomicString->string);
    pthread_mutex_unlock(&atomicString->mutex);
  }
}

int main(int argv, char** argc) {
  char loremIpsum[]=
      "Lorem ipsum dolor sit amet, te nusquam fierent ponderum his.\n"
      "Ut vel integre discere posidonium, eu mel dictas constituam interpretaris, modus simul propriae pro te. Prima putant insolens in qui,\n"
      "at his virtute sententiae. Ius ut euismod recteque iracundia. Has iusto vituperatoribus in, an nec saperet incorrupte.";
  pthread_t uppercaseThread, lowercaseThread;
  AtomicString atomicString= {loremIpsum, PTHREAD_MUTEX_INITIALIZER};
  int threadStatus;

  threadStatus= pthread_create(&uppercaseThread, NULL, uppercase, &atomicString);
  printf("threadStatus=%d\n", threadStatus);
  threadStatus= pthread_create(&lowercaseThread, NULL, lowercase, &atomicString);
  printf("threadStatus=%d\n", threadStatus);

  getchar();
  running= 0;

  return 0;
}