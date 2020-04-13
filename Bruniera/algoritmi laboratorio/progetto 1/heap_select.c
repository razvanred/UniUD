#include <stdio.h>
#include <stdlib.h>
#include "heap.h"

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
int heap_select(int array[], int size, int k);

int main(int argc, char** argv) {
	int *v;
    unsigned int size; 
    char str[10000];
    unsigned int k;
    
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", heap_select(v, size, k - 1));
    return 0;
}

int heap_select(int array[], int size, int k) {
	int iter;
	Vector primary = int_vector_from(array, size);
	Vector secondary;
	Heap heap;
	Heap ref;
	
	if(k <= size / 2) {
		iter = k;
		secondary = index_vector_new(iter + 1, array);
		heap = minheap_from(&primary);
		ref = minheap_with(&secondary);
	} else {
		iter = size - k - 1;
		secondary = index_vector_new(iter + 1, array);
		heap = maxheap_from(&primary);
		ref = maxheap_with(&secondary);
	}
	
	ref.push(&ref, 0);
	for(; iter > 0; iter--) {
		int temp = ref.pop(&ref);
		if(heap_left(temp) < heap.heapsize) {
			ref.push(&ref, heap_left(temp));
		}
		if(heap_right(temp) < heap.heapsize) {
			ref.push(&ref, heap_right(temp));
		}
	}
	int ret = primary.get(&primary, secondary.get(&secondary, 0));
	
	// Avendo scritto l'implementazione dei distruttori, so che quelli commentati sono superflui
	/*heap.delete(&heap);
	ref.delete(&ref);
	primary.delete(&primary);*/
	secondary.delete(&secondary);
	
	return ret;
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

