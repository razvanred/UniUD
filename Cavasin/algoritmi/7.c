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

// quadratic
Indices poimax1(int v[], unsigned int size) {
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
Indices poimax2(int v[], unsigned int size) {
  Indices d;
  d.i= 0;
  d.j= size - 1;

  int *iBuffer= malloc(size * sizeof(int)), *jBuffer= malloc(size * sizeof(int));
  Indices bufferSize;
  bufferSize.i= 1;
  bufferSize.j= 1;
  iBuffer[0]= d.i;
  jBuffer[0]= d.j;

  for(; d.i < size; d.i++, d.j--) {
    if(v[d.i] < v[iBuffer[bufferSize.i - 1]]) {
      iBuffer[bufferSize.i]= d.i;
      bufferSize.i++;
    }
    if(v[d.j] > v[jBuffer[bufferSize.j - 1]]) {
      jBuffer[bufferSize.j]= d.j;
      bufferSize.j++;
    }
  }

  for(int i= 0, j= 0, ts= v[jBuffer[j]] - v[iBuffer[i]]; j < bufferSize.j; j++) {
    if(v[jBuffer[j]] < v[iBuffer[i]]) {
      i= j;
    }
    if(v[jBuffer[j]] - v[iBuffer[i]] >= ts) {
      ts= v[jBuffer[j]] - v[iBuffer[i]];
      d.i= iBuffer[i];
      d.j= jBuffer[j];
    }
  }

  free(iBuffer);
  free(jBuffer);

  return d;
}

Indices poimax3(int v[], unsigned int size) {
  Indices d;

  for(int i= 0, j= 0, s= v[j] - v[i]; j < size; j++) {
    if(v[j] < v[i])
      i= j;
    if(v[j] - v[i] >= s) {
      s= v[j] - v[i];
      d.i= i;
      d.j= j;
    }
  }
  return d;
}

int main(int argc, char **argv) {
  int *v, size;
  Indices d;
  char s[20];

  printf(">>");
  readLine(s, 20);
  parseArray(&v, &size, s);

  // d= poimax1(v, size);
  // printf("%d %d\n", d.i, d.j);

  // d= poimax2(v, size);
  // printf("%d %d\n", d.i, d.j);

  d= poimax3(v, size);
  printf("%d %d\n", d.i, d.j);

  // getch();
  free(v);
  return 0;
}