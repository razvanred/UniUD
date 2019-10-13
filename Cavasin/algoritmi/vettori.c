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
}