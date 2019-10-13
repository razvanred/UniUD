#include <stdio.h>

//find the index of an element in a sorted range of an ARRAY by splitting it and checking if ->
//the element is either in the upper lower or middle position of the range or in the element is in ->
//the upper or lower half (greater or smaller than the middle element) and repeat till it's found or out of range
int binary_search_iter(int ARRAY[], int element, int size) {                                 
	int lower = 0;
	int upper = size - 1;
	int position = -1;
	
	while((lower <= upper) && (element >= ARRAY[lower] || element <= ARRAY[upper]) && (position == -1)) { //repeat until either the range is empty (invalid), the element is out of range, the element is found 
		
		if(ARRAY[lower] == element) {                                                        //check if the element is in the upper or lower position, if so saves position
			position = lower;
		} else if (ARRAY[upper] == element) {
			position = upper;
		} else {
			int middle = (lower + upper) / 2;
			
			if(ARRAY[middle] == element) {                                                   //check if the  element is in the middle position, if so saves position
				position = middle;
			}
			if(ARRAY[middle] > element) {                                                    //otherwise set the range to the lower or upper half range excluding already checked posirion
				upper = middle - 1;
				lower++;
			} else {
				lower = middle + 1;
				upper--;
			}
		}
	}
	return position;                                                                          //if the element was found returns it position, if it wasn't the variable still contains -1
}




//find the index of an element in a sorted range of an ARRAY by splitting it and checking if ->
//the element is in the upper lower or middle position of the range or ifthe element is in ->
//the upper or lower half (greater or lower than the middle element) and repeat till it's found or out
int binary_search_tail(int ARRAY[], int element, int lower, int upper, int middle) {
	return 
		(lower > upper)?                                                                                            //check bounduaries (-1 if range is empty or invalid)
			-1:
			(element < ARRAY[lower] || element > ARRAY[upper])?                                                     //check if element is out of range (return -1, element not present)
				-1:
				(ARRAY[lower] == element)?                                                                          //look for element in upper and lower position (return position if found)
					lower:
					(ARRAY[upper] == element)?
						upper:
						(ARRAY[middle] == element)?                                                                 //look for element in middle position (return position if found)
							middle:
							(ARRAY[middle] > element)?
								binary_search_tail(ARRAY, element, lower + 1, middle - 1, (lower + middle) / 2):    //recurse on upper or lower half excluding alredy checked positions
								binary_search_tail(ARRAY, element, middle + 1, upper - 1, (middle + upper) / 2);}

int binary_search_rec(int ARRAY[], int element, int size) {
	return binary_search_tail(ARRAY, element, 0, size - 1, (size - 1) / 2);                                         //call recursing function with the entire vector as range
}





////////////////////////////////////////////////////////////////////////
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
	printf("7 --> rec: %d; iter: %d\n", binary_search_rec(NULL, 7, 0), binary_search_iter(NULL, 7, 0));
	
	printf("\nsingle element array:\n");
	int SINGLE[] = {4};
	println_array(SINGLE, 1);
	printf("4 --> rec: %d; iter: %d\n", binary_search_rec(SINGLE, 4, 1), binary_search_iter(SINGLE, 4, 1));
	printf("5 --> rec: %d; iter: %d\n", binary_search_rec(SINGLE, 5, 1), binary_search_iter(SINGLE, 5, 1));
	
	printf("\neven array :\n");
	int EVEN[] = {2, 5, 7, 23, 36, 68};
	println_array(EVEN, 7);
	printf("2 --> rec: %d; iter: %d\n", binary_search_rec(EVEN, 2, 6), binary_search_iter(EVEN, 2, 6));
	printf("68 --> rec: %d; iter: %d\n", binary_search_rec(EVEN, 68, 6), binary_search_iter(EVEN, 68, 6));
	printf("7 --> rec: %d; iter: %d\n", binary_search_rec(EVEN, 7, 6), binary_search_iter(EVEN, 7, 6));
	printf("70 --> rec: %d; iter: %d\n", binary_search_rec(EVEN, 70, 6), binary_search_iter(EVEN, 70, 6));
	printf("9 --> rec: %d; iter: %d\n", binary_search_rec(EVEN, 9, 6), binary_search_iter(EVEN, 9, 6));
	
	printf("\nodd array :\n");
	int ODD[] = {2, 5, 7, 9, 23, 36, 68};
	println_array(ODD, 7);
	printf("2 --> rec: %d; iter: %d\n", binary_search_rec(ODD, 2, 7), binary_search_iter(ODD, 2, 7));
	printf("68 --> rec: %d; iter: %d\n", binary_search_rec(ODD, 68, 7), binary_search_iter(ODD, 68, 7));
	printf("7 --> rec: %d; iter: %d\n", binary_search_rec(ODD, 7, 7), binary_search_iter(ODD, 7, 7));
	printf("1 --> rec: %d; iter: %d\n", binary_search_rec(ODD, 1, 7), binary_search_iter(ODD, 1, 7));
	printf("22 --> rec: %d; iter: %d\n", binary_search_rec(ODD, 22, 7), binary_search_iter(ODD, 22, 7));
	
	printf("\nlong array:\n (array of size 100000 containing the first 100k even numbers)\n");
	int size = 100000;
	int LONG[size];
	for (int i = 0; i < size; i++){
		LONG[i] = i * 2;
	}
	printf("0 --> rec: %d; iter: %d\n", binary_search_rec(LONG, 0, size), binary_search_iter(LONG, 0, size));
	printf("199998 --> rec: %d; iter: %d\n", binary_search_rec(LONG, (size * 2) -2 , size), binary_search_iter(LONG, (size * 2) -2 , size));
	printf("62 --> rec: %d; iter: %d\n", binary_search_rec(LONG, 62, size), binary_search_iter(LONG, 62, size));
	printf("-1 --> rec: %d; iter: %d\n", binary_search_rec(LONG, -1, size), binary_search_iter(LONG, -1, size));
	printf("200000 --> rec: %d; iter: %d\n", binary_search_rec(LONG, size * 2, size), binary_search_iter(LONG, size * 2, size));
	printf("63 --> rec: %d; iter: %d\n", binary_search_rec(LONG, 9, size), binary_search_iter(LONG, 9, size));
}
