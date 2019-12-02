#include <stdio.h>

int main() {
  int i = 0, n = 0;

  for(int c = getchar(); c != EOF; c = getchar())
  {
    if(c != ' ' && c != '\t' && c != '\n') {
      n++;
    } else {

      for(i = 0; i < n; i++)
        printf("-");

      if(n > 0)
        printf("\n");

      n = 0;
    }
  }

  return 0;
}