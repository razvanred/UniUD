#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct sHeap {
  int *v;
  unsigned int size, heapSize;
} Heap;

void printArray(int v[], unsigned int size) {
  printf("{");
  for(unsigned int i= 0; i < size; i++) {
    printf(" %d", v[i]);
  }
  puts(" }");
}

generateArray(int **v, unsigned int *size) {
  *size= rand() % 50 + 1;
  free(*v);
  *v= malloc(sizeof(int) * *size);
  for(unsigned int i= 0; i < *size; i++) {
    (*v)[i]= rand() % 201 - 100;
  }
}

int *copyArray(int **t, int v[], unsigned int size) {
  free(*t);
  *t= malloc(sizeof(int) * size);
  memcpy(*t, v, sizeof(int) * size);
  return *t;
}

void swap(int v[], unsigned int i, unsigned int j) {
  if(i != j) {
    v[i]^= v[j];
    v[j]= v[i] ^ v[j];
    v[i]^= v[j];
  }
}

unsigned int heapLeft(unsigned int i) {
  return 2 * (i + 1) - 1;
}

unsigned int heapRight(unsigned int i) {
  return 2 * (i + 1);
}

unsigned int heapParent(unsigned int i) {
  return i / 2;
}

void heapify(Heap h, unsigned int i) {
  unsigned int l= heapLeft(i), r= heapRight(i), n;
  if(l >= h.heapSize)
    l= i;
  if(r >= h.heapSize)
    r= i;
  if(h.v[l] > h.v[r])
    n= l;
  else
    n= r;
  if(h.v[i] > h.v[n])
    n= i;
  if(i != n) {
    swap(h.v, i, n);
    heapify(h, n);
  }
}

Heap generateHeap(int v[], unsigned int size) {
  Heap h;
  h.size= size;
  if(v) {
    h.heapSize= size;
    h.v= v;
    for(unsigned int i= h.heapSize / 2; i > 0; i--) {
      heapify(h, i - 1);
    }
  } else {
    h.heapSize= 0;
    h.v= malloc(size * sizeof(int));
  }
  return h;
}

int pop(Heap *h) {
  if((*h).heapSize > 0) {
    (*h).heapSize--;
    swap((*h).v, 0, (*h).heapSize);
    heapify((*h), 0);
    return (*h).v[(*h).heapSize];
  } else
    return 0;
}

void push(Heap *h, int n) {
  if((*h).heapSize < (*h).size) {
    (*h).v[(*h).heapSize]= n;
    for(unsigned int i= (*h).heapSize; i > 0 && (*h).v[i] > (*h).v[heapParent(i)];) {
      swap((*h).v, i, heapParent(i));
      i= heapParent(i);
    }
    (*h).heapSize++;
  }
}

void printHeap(Heap *h, unsigned int i) {
  unsigned int t;
  // printf("size=%d\n", h.heapSize);

  putchar('L');
  for(unsigned j= 0; j < i; j++)
    putchar('-');
  printf("%d\n", (*h).v[i]);

  t= heapLeft(i);
  if(t < (*h).heapSize)
    printHeap(h, t);
  t= heapRight(i);
  if(t < (*h).heapSize)
    printHeap(h, t);
}

void test1() {
  unsigned int size= rand() % 50 + 1;
  Heap h= generateHeap(NULL, size);

  puts("HEAP-BASED PRIORITY QUEUE");

  for(unsigned int i= 0; i < size; i++) {
    int n= rand() % 201 - 100;
    printf("add(%d)\n", n);
    push(&h, n);
    printHeap(&h, 0);
  }
  for(unsigned int i= 0; i < size; i++) {
    printf("remove()\n");
    pop(&h);
    printHeap(&h, 0);
  }

  puts("\n---------------");
}

int main() {
  time_t vtime;
  srand((unsigned)time(&vtime));

  while(1) {
    test1();

    getch();
  }

  return 0;
}