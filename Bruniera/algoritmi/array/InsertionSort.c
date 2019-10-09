#include <stdio.h>

void insertion_sort(int A[], int length) { //InsertionSort
	for(int j = 1; j < length; j++){
		int key = A[j];  		//memorizzo il primo valore non ordinato 
		int i = j - 1;
		
		while(i >= 0 && A[i] > key) {   //sposto di uno tutti quelli maggiori del valore non ordinato
			A[i + 1] = A[i];
			i--;
		}
		
		A[i + 1] = key;           //inserisco il valore ordinato
	}
}




void println_array(int A[], int length){
	for(int i = 0; i < length; i++){
		printf("%d ", A[i]);
	}
	printf("\n");
}


int main(int argc, char **argv) { 
	printf("empty array:\n");
	println_array(NULL, 0);
	insertion_sort(NULL, 0);
	println_array(NULL, 0);
	
	printf("\nsingle element array:\n");
	int SINGLE[] = {4};
	println_array(SINGLE, 1);
	insertion_sort(SINGLE, 1);
	println_array(SINGLE, 1);
	
	printf("\nbest case :\n");
	int BEST[] = {2, 5, 7, 9, 23, 36, 68};
	println_array(BEST, 7);
	insertion_sort(BEST, 7);
	println_array(BEST, 7);
	
	printf("\nworst case :\n");
	int WORST[] = {68, 36, 23, 9, 7, 5, 2};
	println_array(WORST, 7);
	insertion_sort(WORST, 7);
	println_array(WORST, 7);
	
	printf("\nrandom array :\n");
	int RANDOM[] = {43, 234, 87, 2, 8, 34, 12};
	println_array(RANDOM, 7);
	insertion_sort(RANDOM, 7);
	println_array(RANDOM, 7);
}

