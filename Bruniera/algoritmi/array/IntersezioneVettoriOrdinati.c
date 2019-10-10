#include <stdio.h>

//takes in input two sorted arrays and their size, prints in line their
//intersection, ends with a space and new line
void println_intersection(int A[], int sizeA, int B[], int sizeB) {
	int i=0;                          //initialize indexes to start scrolling through both arrays starting from 0
	int j=0;
	
	while(i < sizeA && j < sizeB) {   //stops when one array is ended, there cant be other element in common
		if(A[i] == B[j]) {
			printf("%d ", A[i]);       //if two elements are in common print them and increse both indexes
			i++;
			j++;
		} else if(A[i] < B[j]) {      //if element in one array is smaller increase it's index ->
			i++;                      //the next element will be equal or greater and possibly ->
		} else {                      //equal to the one in the other array
			j++;
		}
	}
	
	printf("\n");                     //ends with a new line
}






////////////////////////////////////////////////////////////////////////
// tests

void println_array(int A[], int length){
	for(int i = 0; i < length; i++){
		printf("%d ", A[i]);
	}
	printf("\n");
}

int main(int argc, char** argv) {
	printf("empty arrays:\n");
	printf("A-> ");
	println_array(NULL, 0);
	printf("B-> ");
	println_array(NULL, 0);
	printf("intersection-> ");
	println_intersection(NULL, 0, NULL, 0);
	
	
	printf("\none empty array:\n");
	int A[] = {1, 2, 3, 4, 5, 6, 7};
	printf("A-> ");
	println_array(A, 7);
	printf("B-> ");
	println_array(NULL, 0);
	printf("intersection-> ");
	println_intersection(A, 7, NULL, 0);
	
	printf("\n same array:\n");
	printf("A-> ");
	println_array(A, 7);
	printf("A-> ");
	println_array(A, 7);
	printf("intersection-> ");
	println_intersection(A, 7, A, 7);
	
	printf("\nequal arrays:\n");
	printf("A-> ");
	println_array(A, 7);
	int B[] = {1, 2, 3, 4, 5, 6, 7};
	printf("B-> ");
	println_array(B, 7);
	printf("intersection-> ");
	println_intersection(A, 7, B, 7);
	
	printf("\nworst case:\n");
	int A1[] = {1, 3, 5, 7, 9, 11, 13};
	printf("A-> ");
	println_array(A1, 7);
	int B1[] = {2, 4, 6, 8, 10, 12, 14};
	printf("B-> ");
	println_array(B1, 7);
	printf("intersection-> ");
	println_intersection(A1, 7, B1, 7);
	
	printf("\nbest case:\n");
	printf("A-> ");
	println_array(A, 7);
	int B2[] = {8, 9, 10, 11, 12, 13, 14};
	printf("B-> ");
	println_array(B2, 7);
	printf("intersection-> ");
	println_intersection(A, 7, B2, 7);
	
	printf("\narrays with repeating elements:\n");
	int A2[] = {1, 3, 5, 5, 7, 9, 9, 11, 13};
	printf("A-> ");
	println_array(A2, 9);
	int B3[] = {2, 3, 3, 4, 5, 5, 6, 7, 11};
	printf("B-> ");
	println_array(B3, 9);
	printf("intersection-> ");
	println_intersection(A2, 9, B3, 9);
}
