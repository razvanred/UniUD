#include <stdio.h>
#include <string.h>

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

int main(int argc, char **argv) {
  char s[20];

  printf(">>");
  readLine(s, 20);
  for(unsigned int i= 0; i < strlen(s); i++) {
    printf("%s\n", &s[i]);
  }
  getch();
  return 0;
}