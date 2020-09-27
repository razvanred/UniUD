#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct sNode {
  int n;
  struct sNode *last, *next;
} Node;

typedef struct sRangedArray {
  int v[100], i, size;
} RangedArray;

void printRangedArray(RangedArray v) {
  printf("{");
  for(; v.size > 0; v.size--, v.i++ % 100) {
    printf(" %d", v.v[v.i]);
  }
  puts(" }");
}

void printList(Node *head) {
  printf("{");
  while(head != NULL) {
    printf(" %d", head->n);
    head= head->next;
  }
  puts(" }");
}

RangedArray generateRangedArray() {
  RangedArray v;
  v.i= 0;
  v.size= 0;
  return v;
}

Node *createNode(int n) {
  Node *node= malloc(sizeof(Node));
  node->n= n;
  node->next= NULL;
  node->last= node;

  return node;
}

void listQueue_add(Node **head, Node *node) {
  if(head != NULL && (*head) != NULL) {
    node->last= NULL;
    (*head)->last->next= node;
    (*head)->last= node;
  } else {
    (*head)= node;
  }
}

Node listQueue_remove(Node **head) {
  Node node;

  if(head != NULL && (*head) != NULL) {
    node= **head;
    if((*head)->next != NULL) {
      (*head)->next->last= (*head)->last;
      free(*head);
      (*head)= node.next;
    } else {
      (*head)= NULL;
    }
  } else {
    node.next= &node;
  }

  return node;
}

void listStack_push(Node **head, Node *node) {
  if(head != NULL && (*head) != NULL) {
    (*head)->last= NULL;
    node->next= (*head);
    node->last= (*head)->last;
  }
  (*head)= node;
}

Node listStack_pop(Node **head) {
  Node node;

  if(head != NULL && (*head) != NULL) {
    node= **head;
    if((*head)->next != NULL) {
      (*head)->next->last= (*head)->last;
      free(*head);
      (*head)= node.next;
    } else {
      (*head)= NULL;
    }
  } else {
    node.next= &node;
  }

  return node;
}

void arrayQueue_add(RangedArray *v, int n) {
  if(v->size < 100) {
    v->v[(v->i + v->size) % 100]= n;
    v->size++;
  }
}

int arrayQueue_remove(RangedArray *v) {
  int i= 0;

  if(v->size > 0) {
    i= v->v[v->i];
    v->size--;
    v->i= v->i++ % 100;
  }
  return i;
}

void arrayStack_push(RangedArray *v, int n) {
  if(v->size < 100) {
    v->v[(v->i + v->size) % 100]= n;
    v->size++;
  }
}

int arrayStack_pop(RangedArray *v) {
  int i= 0;

  if(v->size > 0) {
    i= v->v[v->i + v->size - 1];
    v->size--;
  }
  return i;
}

void test3() {
  int size= rand() % 50 + 1;
  Node *a= NULL;

  puts("LIST-BASED QUEUE, LIST-BASED STACK");

  for(int i= 0, n; i < size; i++) {
    n= rand() % 101;
    printf("listQueue_add(%d):\t", n);
    listQueue_add(&a, createNode(n));
    printList(a);
  }
  for(int i= 0; i < size; i++) {
    printf("listQueue_remove():\t");
    listQueue_remove(&a);
    printList(a);
  }
  for(int i= 0, n; i < size; i++) {
    n= rand() % 101;
    printf("listStack_push(%d):\t", n);
    listStack_push(&a, createNode(n));
    printList(a);
  }
  for(int i= 0; i < size; i++) {
    printf("listStack_pop:\t");
    listStack_pop(&a);
    printList(a);
  }

  puts("\n---------------");
}

void test4() {
  int size= rand() % 50 + 1;
  RangedArray v= generateRangedArray();

  puts("ARRAY-BASED QUEUE, ARRAY-BASED STACK");

  for(int i= 0, n; i < size; i++) {
    n= rand() % 101;
    printf("arrayQueue_add(%d):\t", n);
    arrayQueue_add(&v, n);
    printRangedArray(v);
  }
  for(int i= 0; i < size; i++) {
    printf("arrayQueue_remove():\t");
    arrayQueue_remove(&v);
    printRangedArray(v);
  }
  for(int i= 0, n; i < size; i++) {
    n= rand() % 101;
    printf("arrayStack_push(%d):\t", n);
    arrayStack_push(&v, n);
    printRangedArray(v);
  }
  for(int i= 0; i < size; i++) {
    printf("arrayStack_pop:\t");
    arrayStack_pop(&v);
    printRangedArray(v);
  }

  puts("\n---------------");
}

int main() {
  time_t vtime;
  srand((unsigned)time(&vtime));

  while(1) {
    test3();
    test4();

    getch();
  }

  return 0;
}