#include <stdio.h>
#include <stdlib.h>
#include "heap.h"

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
void swap(int array[], int first, int second);
int heap_select(int array[], int size, int k);
void heapify_min(int array[], int size, int root);
void heapify_max(int array[], int size, int root);
int left(int root);
int right(int root);
int parent(int root);

int main(int argc, char** argv) {
	/*printf("%d %d %d %d\n", parent(1), parent(2), left(0), right(0));
	printf("%d %d %d %d\n", parent(3), parent(4), left(1), right(1));
	printf("%d %d %d %d\n", parent(5), parent(6), left(2), right(2));
	printf("%d %d %d %d\n", parent(13), parent(14), left(6), right(6));
	printf("%d %d %d %d\n", parent(11), parent(12), left(5), right(5));*/
	
	Vector v = int_vector_new(5);
	v.set(&v, 0, (void*)5);
	v.set(&v, 1, (void*)6);
	v.set(&v, 2, (void*)7);
	v.set(&v, 3, (void*)8);
	v.set(&v, 4, (void*)5);
	int test1 = v.compare(&v, 0, 1);
	int test2 = v.compare(&v, 0, 4);
	int test3 = v.compare(&v, 1, 0);
	v.swap(&v, 1, 3);
	int a = (int)v.get(&v, 0);
	int b = (int)v.get(&v, 1);
	int c = (int)v.get(&v, 2);
	int d = (int)v.get(&v, 3);
	int e = (int)v.get(&v, 4);
	
	Vector i = index_vector_new(2, (int*)v.data);
	i.set(&i, 0, 3);
	i.set(&i, 1, 4);
	int test4 = i.compare(&i, 0, 1);
	i.swap(&i, 0, 1);
	int test5 = i.compare(&i, 0, 1);
	i.set(&i, 1, 0);
	int test6 = i.compare(&i, 0, 1);
	int f = (int)i.get(&i, 0);
	int g = (int)i.get(&i, 1);
	
	index_vector_delete(&i);
	int_vector_delete(&v);
	
	printf("%d %d %d\n%d %d %d %d %d\n%d %d %d\n%d %d\n", test1, test2, test3, a, b, c, d, e, test4, test5, test6, f, g);
	
	/*int *v;
    unsigned int size; 
    char str[10000];
    unsigned int k;
    
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", heap_select(v, size, k - 1));*/
    return 0;
}

/*void heapify_maxheap(struct Heap* heap, int root) {
	int max = root;
	if(left_child(root) <= heap->heapsize && get(heap, max) < get(heap, left_child(root))) { 
		max = left_child(root);
	}
	if(right_child(root) <= heap->heapsize && get(heap, max) < get(heap, right_child(root))) {
		max = right_child(root);
	}
	
	if(max != root) {
		swap_heap(heap, root, max);
		heapify_maxheap(heap, max);
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

int heap_select(int array[], int size, int k) {
	return 0;
}

