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

void printArray(int v[], int size) {
  printf("{");
  for(int i= 0; i < size; i++) {
    printf(" %d", v[i]);
  }
  puts(" }");
}

int main(int argc, char **argv) {
  unsigned int size;
  int *v;
  char s[20];

  printf(">>");
  readLine(s, 20);
  parseArray(&v, &size, s);

  // d= aigs1(v, size);
  // printf("%d %d\n", d.i, d.j);

  // getch();
  free(v);
  return 0;
}