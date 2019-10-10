#include <stdio.h>

//find the next (smallest) element to swap, or returns -1 if slready sorted (no element to swap)
int next_swap(int ARRAY[], int p, int q) {
	int sorted = 1;
	int smallest = p;
	for(int i = p+1; i <= q; i++) {
		if(ARRAY[i] < ARRAY[smallest]) {
			smallest = i;
		}
		if(ARRAY[i - 1] > ARRAY[i]) {
			sorted = 0;
		}
	}
	
	return 
		(sorted)?
		-1:
			smallest;
		
	
}

//swap element ARRAY[i] with element ARRAY[j]
void swap(int ARRAY[], int i, int j) {  
	int temp = ARRAY[i];
	ARRAY[i] = ARRAY[j];
	ARRAY[j] = temp;
}

void selection_sort(int ARRAY[], int p, int q) {
	int next;
	if(p < q){
		next = next_swap(ARRAY, p, q);
		if (next > -1){
			swap(ARRAY, p, next);
			selection_sort(ARRAY, p+1, q);
		}
	}
}




//////////////////////////////////////////////////////////////
// tests

void println_array(int A[], int length){
	for(int i = 0; i < length; i++){
		printf("%d ", A[i]);
	}
	printf("\n");
}


int main(int argc, char **argv) { 
	printf("empty array:\n");
	println_array(NULL, 0);
	selection_sort(NULL, 0, -1);  // non ha senso
	println_array(NULL, 0);
	
	printf("\nsingle element array:\n");
	int SINGLE[] = {4};
	println_array(SINGLE, 1);
	selection_sort(SINGLE, 0, 0);
	println_array(SINGLE, 1);
	
	printf("\nordered array :\n");
	int ORDER[] = {2, 5, 7, 9, 23, 36, 68};
	println_array(ORDER, 7);
	selection_sort(ORDER, 0, 6);
	println_array(ORDER, 7);
	
	printf("\nreverse array :\n");
	int REVERSE[] = {68, 36, 23, 9, 7, 5, 2};
	println_array(REVERSE, 7);
	selection_sort(REVERSE, 0, 6);
	println_array(REVERSE, 7);
	
	printf("\nrandom array :\n");
	int RANDOM[] = {43, 234, 87, 2, 8, 34, 12, 3, 54, 23, 76, 12, 89, 84, 76, 23, 543, 534, 26, 85, 12, 84, 27, 678, 27, 53, 50, 163, 526, 764, 652, 274, 359, 637, 58};
	println_array(RANDOM, 35);
	selection_sort(RANDOM, 0, 34);
	println_array(RANDOM, 35);
}
