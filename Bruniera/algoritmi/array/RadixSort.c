#include <stdio.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// declaration

void radix_sort(int array[], int size);
void radix_sort_half(int array[], int lower, int upper);
int sign_partition(int array[], int size);
void swap(int array[], int first, int second);
void swap_p(int *array[], int first, int second);
int hex_counting_sort(int array[], int spare[], int size, int map, int shift);



////////////////////////////////////////////////////////////////////////
// test

void println_array(int a[], int length){
	for(int i = 0; i < length; i++){
		printf("%d ", a[i]);
	}
	printf("\n");
}

int main(int argc, char **argv) {
	int array[] = {23, 34, 45, 12, 32, 10, 2, 5, 6, 2, 45};
	
	println_array(array, 11);
	radix_sort(array, 11);
	println_array(array, 11);
	
	printf("\n");
	
	int array1[] = {1, 3, -0, 5, 7, 2, 9, 8, -6, 4, -6, 10, 12, 65, 43, -67, 78, -23, 56, 89, 52, 46, -18, 30, 40, 72, -40};
	
	println_array(array1, 27);
	radix_sort(array1, 27);
	println_array(array1, 27);
	
	printf("\n");
	
	int array2[] = {23, 456, 5, -76, 43, 23, 456, 78, 7, 654, 32, 3, 456, 78, 765, 43, 2, 345, 678, 987, 654, 32, 34, 567, 8, 7654, -98, 32, 345, 678, 9, 87, 65, 432, 34, 56, 78, 9, -1, 87, 654, 32, 345, 678, 98, 765, 43, 212, 345, 6789, 8, 7654, 32, 3456, 789, 0, 9, 876, 5432, 345, 6789, 9, 876, 54, 32, 3, 456, 7, 890, 9, 7654, 323, 456, 78, 90, 987, 2345, 678, 9, 87, 654, 32, 3, 4567, 89, 98, 765, 432, 3, -180, 45, 67, 890, 987, 654, 32, 345, 678, 9, 0, 9, 876, 5432, 34, 567, 89, 9, 87, 654, 32, 34, 5678, 90, 98, 765, 43, 23, 4567, 89, 876, 543, 234, 567, 89, 987, 654, 32, 34, 5678, 90, 98, 76, 543, 23, 45, 67, 8, 90, 9, 876, 54, 32, 345, 6789, 0, 98, 76, 543, 23, 456, 789, 876, 23, 456, 5, 43, 23, 456, 78, 7, 654, 32, 3, 456, 78, 765, 43, 2, 345, 678, 987, 654, 32, 34, 567, 8, 7654, 32, 345, 678, 9, 87, 65, 432, 34, 56, 78, 9, 87, 654, 32, 345, 678, 98, 765, 43, 212, 345, 6789, 8, 7654, 32, 3456, 789, 0, 9, 876, 5432, 345, 6789, 9, 876, 54, 32, 3, 456, 7, 890, 9, 7654, 323, 456, 78, 90, 987, 2345, 678, 9, 87, 654, 32, 3, 4567, 89, 98, 765, 432, 3, 45, 67, 890, 987, 654, 32, 345, 678, 9, 0, 9, 876, 5432, 34, 567, 89, 9, 87, 654, 32, 34, 5678, 90, 98, 765, 43, 23, 4567, 89, 876, 543, 234, 567, 89, 987, 654, 32, 34, 5678, 90, 98, 76, 543, 23, 45, 67, 8, 90, 9, 876, 54, 32, 345, 6789, 0, 98, 76, 543, 23, 456, 789, 876};
	
	println_array(array2, 300);
	radix_sort(array2, 300);
	println_array(array2, 300);
	
	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

int sign_partition(int array[], int size) {
	int break_even = -1;
	
	for(int j = 0; j < size; j++){
		if(array[j] < 0) {
			break_even++;
			swap(array, break_even, j);
		}
	}
	
	return break_even;
}

void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

void swap_p(int *array[], int first, int second) {
	int *temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

void radix_sort(int array[], int size) {
	int break_even = sign_partition(array, size);
	
	radix_sort_half(array, break_even + 1, size - 1);
	radix_sort_half(array, 0, break_even);
}


void radix_sort_half(int array[], int lower, int upper){
	if(upper > lower) {
		int size = (upper - lower + 1);
		int *spare[] = {&array[lower], malloc(sizeof(int) * size)};
		
		int map, shift;
		for(map = 0xf, shift = 0; map > 0; map <<= 4, shift+=4) {
			hex_counting_sort(spare[0], spare[1], size, map, shift);
			swap_p(spare, 0, 1);
		}
		
		hex_counting_sort(spare[0], spare[1], size, 0x7 << shift, shift);
		
		for(int i = 0; i < size; i++) {
			spare[1][i] = spare[0][i];
		}
	}	
}

int hex_counting_sort(int array[], int spare[], int size, int map, int shift) {
	int vec[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	for(int i = 0; i < size; i++){
		vec[(array[i] & map) >> shift]++;
	}
	
	vec[0]--;
	for(int i = 1; i < 16; i++) {
		vec[i] += vec[i - 1];
	}
	
	for(int i = size - 1; i >= 0; i--) {
		spare[vec[(array[i] & map) >> shift]] = array[i];
		vec[(array[i] & map) >> shift]--;
	}
}
