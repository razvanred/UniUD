#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

//1 1 1 0 0 0 0 0 1 1 0 1 0 0 1 1 1 1 0 0
//0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 1 0 1 1 1 0 1 1 0 0 0 1 1 1 0 0 1 1 0 0 1 0 0 0 1 1 1 1 1 1 1 1 0 0 1

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
int partial_majority_candidate(int v[], unsigned int size);
bool majority(int v[], unsigned int size, int element);

int main(int argc, char** argv) {
	int *v;
    char str[1000];
    unsigned int size; 
    
    readline(str, 1000);
    parseArray(&v, &size, str);
    
    int pmc = partial_majority_candidate(v, size);
    
    if(majority(v, size, pmc)) {
    	printf("%d\n", pmc);
	} else {
		puts("No majority");
	}
    
    free(v);
}

int partial_majority_candidate(int v[], unsigned int size) {
	int candidate = v[0];
	int rounds = 1;
	
	for(int i = 0; i < size; i++) {
		if(v[i] == candidate) {
			rounds++;
		} else {
			rounds--;
			if(rounds <= 0) {
				candidate = v[i];
				rounds = 1;
			}
		}
	}
	
	return candidate;
}

bool majority(int v[], unsigned int size, int element) {
	int count = 0;
	
	for(int i = 0; i < size; i++) {
		if(v[i] == element) {
			count++;
		}
	}
	
	return (count > (size / 2));
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
    for(; *s != 0 && *s != ' '; s+= sizeof(char))
      ;
    s+= sizeof(char);
  }
}
