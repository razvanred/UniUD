<<<<<<< HEAD
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printArray(int v[], int size) {
  printf("{");
  for (int i = 0; i < size; i++) {
    printf(" %d", v[i]);
  }
  printf(" }\n");
}

void insertionSort(int v[], int size) {
  for (int t, i, j = 1; j < size; j++) {
    t = v[j];
    for (i = j - 1; i >= 0 && v[i] > t; i--) {
      v[i + 1] = v[i];
    }
    v[i + 1] = t;
  }
}

void mergeSort() {
  
}

int main() {
  int v[6], t[6] = {5, 3, 2, 4, 8, 1};

  insertionSort(v, 6);
  printArray(v, 6);

  return 0;
=======
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

void merge(int a[], int p, int q, int r) {
  int *t= malloc((q - p) * sizeof(int));

  for(int tp= p, tr= r, i= 0; tp < r || tr < q; i++) {
    if(tr == q || (tp < r && a[tp] < a[tr])) {
      t[i]= a[tp];
      tp++;
    } else {
      t[i]= a[tr];
      tr++;
    }
  }
  for(int tp= p, i= 0; tp < q; tp++, i++) {
    a[tp]= t[i];
  }
  free(t);
}

void mergeSort(int a[], int p, int q) {
  int r= (p + q) / 2;

  if(q - p > 2) {
    mergeSort(a, p, r);
    mergeSort(a, r, q);
  }
  merge(a, p, q, r);
}

int main() {
  time_t vtime;
  int *v= NULL, *t= NULL, size;
  srand((unsigned)time(&vtime));

  while(1) {
    generateArray(&v, &size);
    copyArray(&t, v, size);
    printArray(v, size);

    insertionSort(t, size);
    printArray(t, size);
    mergeSort(v, 0, size);
    printArray(v, size);

    printf("->searching v[0]=%d: %d\n", v[0], binarySearch(v, size, v[0]));
    printf("->searching v[%d]=%d: %d\n", size - 1, v[size - 1],
           binarySearch(v, size, v[size - 1]));
    int i= rand() % 50 + 1;
    printf("->searching %d: %d\n", i, binarySearch(v, size, i));
    printf("---------------\n\n");

    getch();
  }

  return 0;
>>>>>>> completed merge sort, phew
}