#include <stdio.h>
#include <stdlib.h>

typedef struct{
	int i;
	int j;
} Pair;

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
Pair max_difference(int v[], unsigned int size);

int main(int argc, char** argv) {
	int *v;
    char str[1000];
    unsigned int size; 
    
    readline(str, 1000);
    parseArray(&v, &size, str);
    
    Pair diff = max_difference(v, size);
	
    printf("%d %d\n", diff.i, diff.j);
    
    free(v);
}

Pair max_difference(int v[], unsigned int size) {
	Pair best = {0, 0};
	Pair candidate = {0, 0};
	
	for(int cur = 0; cur < size; cur++) {
		if(v[cur] > v[best.j]) {
			best = (Pair){candidate.i, (candidate.j = cur)};
		} else if(v[cur] > v[candidate.j]) {
			candidate.j = cur;
			
			if(v[candidate.j] - v[candidate.i] > v[best.j] - v[best.i]) {
				best = candidate;
			}
		} else if(v[cur] < v[candidate.i]) {
			candidate = (Pair){cur, cur};
		}
	}
	
	return best;
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
