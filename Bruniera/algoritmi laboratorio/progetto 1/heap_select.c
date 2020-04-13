#include <stdio.h>
#include <stdlib.h>
#include "heap.h"

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
int heap_select(int array[], int size, int k);

int main(int argc, char** argv) {
	/*printf("%d %d %d %d\n", parent(1), parent(2), left(0), right(0));
	printf("%d %d %d %d\n", parent(3), parent(4), left(1), right(1));
	printf("%d %d %d %d\n", parent(5), parent(6), left(2), right(2));
	printf("%d %d %d %d\n", parent(13), parent(14), left(6), right(6));
	printf("%d %d %d %d\n", parent(11), parent(12), left(5), right(5));*/
	
	/*Vector v = int_vector_new(5);
	v.set(&v, 0, 5);
	v.set(&v, 1, 6);
	v.set(&v, 2, 7);
	v.set(&v, 3, 8);
	v.set(&v, 4, 5);
	/*int test1 = v.compare(&v, 0, 1);
	int test2 = v.compare(&v, 0, 4);
	int test3 = v.compare(&v, 1, 0);
	v.swap(&v, 1, 3);
	int a = v.get(&v, 0);
	int b = v.get(&v, 1);
	int c = v.get(&v, 2);
	int d = v.get(&v, 3);
	int e = v.get(&v, 4);
	
	Vector i = index_vector_new(2, v.data);
	i.set(&i, 0, 3);
	i.set(&i, 1, 4);
	int test4 = i.compare(&i, 0, 1);
	i.swap(&i, 0, 1);
	int test5 = i.compare(&i, 0, 1);
	i.set(&i, 1, 0);
	int test6 = i.compare(&i, 0, 1);
	int f = i.get(&i, 0);
	int g = i.get(&i, 1);
	printf("%d %d %d\n%d %d %d %d %d\n%d %d %d\n%d %d\n", test1, test2, test3, a, b, c, d, e, test4, test5, test6, f, g);*/
	
	/*Heap h = maxheap_from(&v);
	
	int a = v.get(&v, 0);
	int b = v.get(&v, 1);
	int c = v.get(&v, 2);
	int d = v.get(&v, 3);
	int e = v.get(&v, 4);
	
	printf("%d %d %d %d %d\n", a, b, c, d, e);
	
	a = h.pop(&h);
	b = h.pop(&h);
	c = h.pop(&h);
	d = h.pop(&h);
	e = h.pop(&h);
	
	printf("pop: %d %d %d %d %d\n", a, b, c, d, e);
	
	printf("%d %d %d %d %d\n", v.get(&v, 0), v.get(&v, 1), v.get(&v, 2), v.get(&v, 3), v.get(&v, 4));
	
	h.push(&h, 1);
	printf("1: %d %d %d %d %d\n", v.get(&v, 0), v.get(&v, 1), v.get(&v, 2), v.get(&v, 3), v.get(&v, 4));
	h.push(&h, 7);
	printf("7: %d %d %d %d %d\n", v.get(&v, 0), v.get(&v, 1), v.get(&v, 2), v.get(&v, 3), v.get(&v, 4));
	h.push(&h, 3);
	printf("3: %d %d %d %d %d\n", v.get(&v, 0), v.get(&v, 1), v.get(&v, 2), v.get(&v, 3), v.get(&v, 4));
	h.push(&h, 5);
	printf("5: %d %d %d %d %d\n", v.get(&v, 0), v.get(&v, 1), v.get(&v, 2), v.get(&v, 3), v.get(&v, 4));
	h.push(&h, 2);
	printf("2: %d %d %d %d %d\n", v.get(&v, 0), v.get(&v, 1), v.get(&v, 2), v.get(&v, 3), v.get(&v, 4));
	
	v.delete(&v);
	h.delete(&h);*/
	//i.delete(&i);
	
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
	Vector primary = int_vector_from(array, size);
	Vector secondary = index_vector_new(k + 1, array);
	Heap heap = minheap_from(&primary);
	Heap ref = minheap_with(&secondary);
	
	ref.push(&ref, 0);
	
	for(; k > 0; k--) {
		int temp = ref.pop(&ref);
		ref.push(&ref, heap_left(temp));
		ref.push(&ref, heap_right(temp));
	}
	
	int ret = primary.get(&primary, secondary.get(&secondary, 0));
	
	primary.delete(&primary);
	secondary.delete(&secondary);
	heap.delete(&heap);
	ref.delete(&ref);
	
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

