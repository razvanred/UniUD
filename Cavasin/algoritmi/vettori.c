//#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printArray(int v[], int size) {
  printf("{");
  for(int i= 0; i < size; i++) {
    printf(" %d", v[i]);
  }
  printf(" }\n");
}

generateArray(int **v, int *size) {
  *size= rand() % 50 + 1;
  free(*v);
  *v= malloc(sizeof(int) * *size);
  for(int i= 0; i < *size; i++) {
    (*v)[i]= rand() % 100;
  }
}

int *copyArray(int **t, int v[], int size) {
  free(*t);
  *t= malloc(sizeof(int) * size);
  memcpy(*t, v, sizeof(int) * size);
  return *t;
}

int binarySearch(int v[], int size, int k) {
  int pivot= size / 2;

  // printArray(v, size);
  // printf("[%d]=%d\n", pivot, v[pivot]);

  if(k == v[pivot]) {
    return 1;
  } else if(size == 1) {
    return 0;
  } else if(k < v[pivot]) {
    return binarySearch(v, pivot, k);
  } else {
    return binarySearch(v + pivot, size - pivot, k);
  }
}

void insertionSort(int v[], int size) {
  for(int t, i, j= 1; j < size; j++) {
    t= v[j];
    for(i= j - 1; i >= 0 && v[i] > t; i--) {
      v[i + 1]= v[i];
    }
    v[i + 1]= t;
  }
}

void selectionSort(int v[], int size) {
  for(int s= 0, iMin, i; s < size - 1; s++) {
    for(iMin= s, i= s + 1; i < size; i++) {
      if(v[i] < v[iMin]) {
        iMin= i;
      }
    }
    i= v[iMin];
    v[iMin]= v[s];
    v[s]= i;
  }
}

void merge(int v[], int p, int q, int r) {
  int *t= malloc((q - p) * sizeof(int));

  for(int tp= p, tr= r, i= 0; tp < r || tr < q; i++) {
    if(tr == q || (tp < r && v[tp] < v[tr])) {
      t[i]= v[tp];
      tp++;
    } else {
      t[i]= v[tr];
      tr++;
    }
  }
  for(int tp= p, i= 0; tp < q; tp++, i++) {
    v[tp]= t[i];
  }
  free(t);
}

void mergeSort(int v[], int p, int q) {
  int r= (p + q) / 2;

  if(q - p > 2) {
    mergeSort(v, p, r);
    mergeSort(v, r, q);
  }
  merge(v, p, q, r);
}

int commonElements(int **c, int a[], int sizeA, int b[], int sizeB) {
  int i= 0;

  free(*c);
  *c= malloc((sizeA + sizeB) * sizeof(int));
  for(int ia= 0, ib= 0; ia < sizeA && ib < sizeB;) {
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

void test1() {
  int *a= NULL, *b= NULL, size;

  puts("SORTING: INSERTION SORT, MERGE SORT, SELECTION SORT, AND BINARY SEARCH");

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

  printf("->searching v[0]=%d: %d\n", b[0], binarySearch(b, size, b[0]));
  printf("->searching v[%d]=%d: %d\n", size - 1, b[size - 1], binarySearch(b, size, b[size - 1]));
  int i= rand() % 50 + 1;
  printf("->searching %d: %d\n", i, binarySearch(b, size, i));

  puts("\n---------------");
}

void test2() {
  int *a= NULL, *b= NULL, *c= NULL, sizeA, sizeB, sizeC;

  puts("COMMON ELEMENTS");

  generateArray(&a, &sizeA);
  generateArray(&b, &sizeB);
  mergeSort(a, 0, sizeA);
  mergeSort(b, 0, sizeB);
  printArray(a, sizeA);
  printArray(b, sizeB);

  sizeC= commonElements(&c, a, sizeA, b, sizeB);
  printArray(c, sizeC);

  puts("\n---------------");
}

int main() {
  time_t vtime;
  srand((unsigned)time(&vtime));

  while(1) {
    test1();
    test2();

    getch();
  }

  return 0;
}