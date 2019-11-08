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
    for(; *s != 0 && *s != ' '; s++)
      ;
    s++;
  }
}

// nlog(n)
Indices aigs1(int v[], unsigned int size, int k) {
  Indices d;
  d.i= 0;
  d.j= 0;
  int t= v[d.j] - v[d.i];

  for(int i= 0; i < size - 1; i++) {
    for(int j= i; j < size; j++) {
      if(v[j] - v[i] > t) {
        t= v[j] - v[i];
        d.i= i;
        d.j= j;
      }
    }
  }
  return d;
}

// linear
Indices aigs2(int v[], unsigned int size, int k) {
  Indices d;
  d.i= 0;
  d.j= 0;
  unsigned int t= v[d.i];

  while(d.j <= size) {
    if(t == k) {
      return d;
    } else if(t > k) {
      t-= v[d.i];
      d.i++;
    } else {
      d.j++;
      t+= v[d.j];
    }
  }
  d.i= -1;
  d.j= -1;
  return d;
}

int main(int argc, char **argv) {
  int *v, size, k;
  Indices d;
  char s[20];

  printf(">>");
  readLine(s, 20);
  scanf("%d", &k);
  parseArray(&v, &size, s);

  // d= aigs1(v, size);
  // printf("%d %d\n", d.i, d.j);

  d= aigs2(v, size, k);
  printf("%d %d\n", d.i, d.j);

  // getch();
  free(v);
  return 0;
}