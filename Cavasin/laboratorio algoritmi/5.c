#include <stdio.h>
#include <stdlib.h>

void readLine(char s[], unsigned int size) {
  unsigned int i= 0;
  char c;
  size--;
  for(c= getchar(); c != '\n';) {
    if(i < size) {
      s[i]= c;
      i++;
    }
    c= getchar();
  }
  if(i >= 0)
    s[i]= 0;
}

void parseArray(int **v, unsigned int *size, char s[]) {
  *size= 0;
  for(unsigned int i= 0; s[i] != 0; i++) {
    if(s[i] == ' ')
      (*size)++;
  }
  (*size)++;
  *v= malloc(*size * sizeof(int));
  for(unsigned int i= 0; i < *size; i++) {
    sscanf(s, "%d", &((*v)[i]));
    for(; *s != 0 && *s != ' '; s++)
      ;
    s++;
  }
}

int binarySearch(int v[], unsigned int p, unsigned int q, int k) {
  unsigned int pivot= (p + q) / 2;

  if(k == v[pivot]) {
    return pivot;
  } else if(q - p <= 1) {
    return -1;
  } else if(k < v[pivot]) {
    return binarySearch(v, p, pivot, k);
  } else {
    return binarySearch(v, pivot + 1, q, k);
  }
}

int main(int argc, char **argv) {
  unsigned int size;
  int *v, k;
  char s[500];

  printf(">>");
  readLine(s, 500);
  scanf("%d", &k);
  parseArray(&v, &size, s);
  printf("%d", binarySearch(v, 0, size, k));
  getch();
  free(v);
  return 0;
}