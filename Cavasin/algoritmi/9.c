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
  for(unsigned int i= 0; s[i] != 0 && s[i + 1] != 0; i++) {
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
int majority1(int v[], unsigned int size, char *status) {
  for(unsigned int c, i= 0; i < size; i++) {
    c= 0;
    for(unsigned int j= 0; j < size; j++) {
      if(v[j] == v[i])
        c++;
    }
    if(c > size / 2) {
      *status= 1;
      return v[i];
    }
  }
  *status= 0;
  return 0;
}

// linear
int majority2(int v[], unsigned int size, char *status) {
  int t;
  for(unsigned int p= 0, i= 0; i < size; i++) {
    if(p == 0)
      t= v[i];
    if(v[i] == t)
      p++;
    else
      p--;
  }
  for(unsigned int i= 0, p= 0; i < size; i++) {
    if(v[i] == t)
      p++;
    if(p > size / 2) {
      *status= 1;
      return t;
    }
  }
  *status= 0;
  return 0;
}

int main(int argc, char **argv) {
  unsigned int size;
  int *v;
  char s[250];

  printf(">>");
  readLine(s, 250);
  parseArray(&v, &size, s);

  {
    int t;
    char s;
    // t= majority1(v, size, &s);
    t= majority2(v, size, &s);
    if(s)
      printf("%d\n", t);
    else
      printf("No majority");
  }

  //   getch();
  free(v);
  return 0;
}