#include <stdio.h>
#include <stdlib.h>

typedef struct sIndices {
  unsigned int i, j;
} Indices;

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
    for(; *s != 0 && *s != ' '; s+= sizeof(char))
      ;
    s+= sizeof(char);
  }
}

// quadratic
Indices poigs1(int v[], unsigned int size, int k) {
  Indices d;
  for(d.i= 0; d.i < size - 1; d.i++) {
    for(d.j= d.i + 1; d.j < size; d.j++) {
      if(v[d.i] + v[d.j] == k) {
        return d;
      }
    }
  }
  d.i= -1;
  d.j= -1;
  return d;
}

// nlog(n)
int binarySearch(int v[], unsigned int p, unsigned int q, int k) {
  int pivot= (p + q) / 2;

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

Indices poigs2(int v[], unsigned int size, int k) {
  Indices d;
  for(d.i= 0; d.i < size - 1; d.i++) {
    d.j= binarySearch(v, d.i + 1, size, k - v[d.i]);
    if(d.j != -1)
      return d;
  }
  d.i= -1;
  d.j= -1;
  return d;
}

// linear
Indices poigs3(int v[], unsigned int size, int k) {
  Indices d;

  for(d.i= 0, d.j= size - 1; d.i < d.j;) {
    if(v[d.i] + v[d.j] == k) {
      return d;
    } else if(v[d.i] + v[d.j] < k) {
      d.i++;
    } else {
      d.j--;
    }
  }
  d.i= -1;
  d.j= -1;
  return d;
}

int main(int argc, char **argv) {
  int *v, size, k;
  Indices d;
  char s[30];

  printf(">>");
  readLine(s, 30);
  scanf("%d", &k);
  parseArray(&v, &size, s);
  d= poigs3(v, size, k);
  printf("%d %d", d.i, d.j);
  getch();
  free(v);
  return 0;
}