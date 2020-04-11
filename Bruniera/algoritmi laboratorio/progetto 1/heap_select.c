#include <stdio.h>
#include <stdlib.h>

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
void swap(int array[], int first, int second);
int heap_select(int array[], int lower, int upper, int k);

int main(int argc, char** argv) {
	int *v;
    unsigned int size; 
    char str[10000];
    unsigned int k;
    
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", heap_select(v, 0, size - 1, k - 1));
}

void readline(char s[], unsigned int size) {
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
  if(i >= 0){
    s[i]= 0;
    i--;
  }
  
  while(s[i] == ' ') {
    s[i] = 0;
    i--;
  }
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
    for(; *s != 0 && *s != ' '; s+= sizeof(char));
    s+= sizeof(char);
  }
}

void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

int heap_select(int array[], int lower, int upper, int k) {
	return 0;
}

