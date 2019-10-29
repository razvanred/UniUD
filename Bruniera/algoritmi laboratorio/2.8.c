#include <stdio.h>
#include <stdlib.h>

/*struct Pair {
    int left;
    int right;
};*/

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
//struct Pair range_between(int v[], int left, int right, int sum);
void vectorize(int v[], int size, int **sums);

int main(int argc, char** argv) {
    int *v;
    int sum, left, right;
    unsigned int size; 
    char str[1000];
    //struct Pair range;
    
    readline(str, 1000);
    parseArray(&v, &size, str);
    
    scanf("%d", &sum);
    
    int **sums;
    sums = malloc(sizeof(int*) * size);
    for(int i = 0; i < size; i++) {
    	sums[i] = malloc(sizeof(int) * (i + 1));
	}
    vectorize(v, size, sums);
    
    //range = range_between(v, 0, size - 1, sum);
    
    left = -1;
    right = -1;
    for(int i = 0; i < size; i++) {
		for(int j = i; j >= 0; j--){
			if(sums[i][j] == sum) {
				left = j;
				right = i;
			}
		}
		free(sums[i]);
	}
	free(sums);
    
    printf("%d %d\n", left, right);
    
    free(v);
    return 0;
}

void vectorize(int v[], int size, int **sums) {
	for(int i = 0; i < size; i++){
		sums[i][i] = v[i];
	}
	
	for(int i = 1; i < size; i++) {
		for(int j = i - 1; j >= 0; j--){
			sums[i][j] = sums[i][j + 1] + v[j];
		}
	}
}

/*struct Pair range_between(int v[], int left, int right, int sum) {
    struct Pair range;
    if(right < left) {
        range.left = -1;
        range.right = -1;
        return range;
    }
    
    int relsum = 0;
    for(int i = left; i <= right; i++) {
        relsum += v[i];
    }
    
    if(sum > relsum) {
        range.left = -1;
        range.right = -1;
        return range;
    }
    
    if(sum == relsum) {
        range.left = left;
        range.right = right;
        return range;
    }
    
    range = range_between(v, left, right - 1, sum);
    if(range.left != -1) {
        return range;
    } else {
        return range_between(v, left + 1, right, sum);
    }
}*/

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
