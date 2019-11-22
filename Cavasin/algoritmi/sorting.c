#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

unsigned int commonElements(int **c, int a[], unsigned int sizeA, int b[], unsigned int sizeB) {
  unsigned int i= 0;

  free(*c);
  *c= malloc((sizeA < sizeB ? sizeA : sizeB) * sizeof(int));
  for(unsigned int ia= 0, ib= 0; ia < sizeA && ib < sizeB;) {
    if(a[ia] == b[ib]) {
      if(i == 0 || (*c)[i - 1] != a[ia]) {
        (*c)[i]= a[ia];
        i++;
      }
      ia++;
      ib++;
    } else if(a[ia] < b[ib]) {
      ia++;
    } else {
      ib++;
    }
  }
  realloc(*c, i * sizeof(int));
  return i;
}

void insertionSort(int v[], unsigned int size) {
  int t;
  for(unsigned int i, j= 1; j < size; j++) {
    t= v[j];
    for(i= j; i > 0 && v[i - 1] > t; i--) v[i]= v[i - 1];
    v[i]= t;
  }
}

void selectionSort(int v[], unsigned int size) {
  for(unsigned int s= 0, i, iMin; s < size - 1; s++) {
    for(iMin= s, i= s + 1; i < size; i++) {
      if(v[i] < v[iMin]) {
        iMin= i;
      }
    }
    if(iMin != s) {
      v[iMin]+= v[s];
      v[s]= v[iMin] - v[s];
      v[iMin]-= v[s];
    }
  }
}

void merge(int v[], unsigned int p, unsigned int q, unsigned int r) {
  int *t= malloc((q - p) * sizeof(int));

  for(unsigned int tp= p, tr= r, i= 0; tp < r || tr < q; i++) {
    if(tr == q || (tp < r && v[tp] < v[tr])) {
      t[i]= v[tp];
      tp++;
    } else {
      t[i]= v[tr];
      tr++;
    }
  }
  for(unsigned int tp= p, i= 0; tp < q; tp++, i++) {
    v[tp]= t[i];
  }
  free(t);
}

void mergeSort(int v[], unsigned int p, unsigned int q) {
  unsigned int r= (p + q) / 2;

  if(q - p > 2) {
    mergeSort(v, p, r);
    mergeSort(v, r, q);
  }
  merge(v, p, q, r);
}

void bubbleSort(int v[], unsigned int size) {
  char flag;
  do {
    flag= 0;
    for(unsigned int i= 1; i < size; i++) {
      if(v[i - 1] > v[i]) {
        v[i]+= v[i - 1];
        v[i - 1]= v[i] - v[i - 1];
        v[i]-= v[i - 1];
        flag= 1;
      }
    }
  } while(flag);
}

unsigned int median(int v[], unsigned int p, unsigned int q) {
  int *t= NULL;
  unsigned int size= q - p, s= 0;

  copyArray(&t, v + p, size);
  for(unsigned int i, iMin; s <= size / 2; s++) {
    for(iMin= s, i= s + 1; i < size; i++) {
      if(t[i] < t[iMin])
        iMin= i;
    }
    if(iMin != s) {
      t[iMin]+= t[s];
      t[s]= t[iMin] - t[s];
      t[iMin]-= t[s];
    }
  }
  for(size= p; t[s - 1] != v[size]; size++)
    ;
  free(t);
  return size;
}

unsigned int select(int v[], unsigned int p, unsigned int q) {
  unsigned int tSize= (q - p + 4) / 5, r;
  int *t[2]= {malloc(sizeof(int) * tSize), malloc(sizeof(int) * tSize)};

  for(unsigned int i= 0; i < tSize; i++) {
    r= p + (i + 1) * 5;
    if(r > q)
      r= q;
    t[0][i]= median(v, p + i * 5, r);
    t[1][i]= v[t[0][i]];
  }
  if(tSize > 1) {
    r= t[0][select(t[1], 0, tSize)];
  } else {
    r= t[0][0];
  }
  free(t[0]);
  free(t[1]);
  return r;
}

unsigned int partition(int v[], unsigned int p, unsigned int q) {
  int pivot= select(v, p, q), i= p - 1;
  if(pivot != q - 1) {
    v[pivot]+= v[q - 1];
    v[q - 1]= v[pivot] - v[q - 1];
    v[pivot]-= v[q - 1];
  }
  pivot= v[q - 1];
  for(unsigned int j= p; j < q; j++) {
    if(v[j] <= pivot) {
      i++;
      if(i != j) {
        v[j]+= v[i];
        v[i]= v[j] - v[i];
        v[j]-= v[i];
      }
    }
  }
  return i;
}

void quickSort(int v[], unsigned int p, unsigned int q) {
  if(p < q) {
    unsigned int r= partition(v, p, q);
    quickSort(v, p, r);
    quickSort(v, r + 1, q);
  }
}

void countingSort(int v[], unsigned int size) {
  int offset= 0, *r;
  unsigned int tSize= 0, *t;

  for(unsigned int i= 0; i < size; i++) {
    if(v[i] < offset)
      offset= v[i];
    if(v[i] > v[tSize])
      tSize= i;
  }
  for(unsigned int i= 0; i < size; i++) {
    v[i]-= offset;
  }
  tSize= v[tSize] + 1;
  t= calloc(tSize, sizeof(unsigned int));
  for(unsigned int i= 0; i < size; i++) {
    t[v[i]]++;
  }
  for(unsigned int i= 1; i < tSize; i++) {
    t[i]+= t[i - 1];
  }
  r= calloc(size, sizeof(int));
  for(unsigned int i= size - 1; i != (unsigned int)-1; i--) {
    r[t[v[i]] - 1]= v[i];
    t[v[i]]--;
  }
  free(t);
  for(unsigned int i= 0; i < size; i++) {
    v[i]= r[i] + offset;
  }
  free(r);
}

void radixSort(int v[], unsigned int size) {
  for(unsigned int i= 0; i < size; i++) {
  }
}

void test1() {
  int *a= NULL, *b= NULL;
  unsigned int size;

  puts("SORTING: INSERTION SORT, MERGE SORT, SELECTION SORT, BUBBLE SORT, QUICK SORT WITH MEDIAN, COUNTING SORT");

  generateArray(&a, &size);
  printArray(a, size);

  copyArray(&b, a, size);
  insertionSort(b, size);
  printArray(b, size);

  copyArray(&b, a, size);
  selectionSort(b, size);
  printArray(b, size);

  copyArray(&b, a, size);
  mergeSort(b, 0, size);
  printArray(b, size);

  copyArray(&b, a, size);
  bubbleSort(b, size);
  printArray(b, size);

  copyArray(&b, a, size);
  quickSort(b, 0, size);
  printArray(b, size);

  copyArray(&b, a, size);
  countingSort(b, size);
  printArray(b, size);

  free(a);
  free(b);
  puts("\n---------------");
}

void test2() {
  int *a= NULL, *b= NULL, *c= NULL;
  unsigned int sizeA, sizeB, sizeC;

  puts("COMMON ELEMENTS");

  generateArray(&a, &sizeA);
  generateArray(&b, &sizeB);
  mergeSort(a, 0, sizeA);
  mergeSort(b, 0, sizeB);
  printArray(a, sizeA);
  printArray(b, sizeB);

  sizeC= commonElements(&c, a, sizeA, b, sizeB);
  printArray(c, sizeC);

  free(a);
  free(b);
  free(c);
  puts("\n---------------");
}

int main() {
  time_t vtime;
  srand((unsigned)time(&vtime));

  while(1) {
    test1();
    // test2();

    getch();
  }

  return 0;
}