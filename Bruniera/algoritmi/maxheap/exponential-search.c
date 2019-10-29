#include <stdio.h>

////////////////////////////////////////////////////////////////////////
// declaration

int exponential_search(int array[], int size, int key);                    //search sorted array in time O(log(i)) where `i` is the output
int binary_search(int array[], int key, int lower, int upper, int middle); //search sorted in time O(log(n)) where `n` is upper-lower



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
	printf("7 --> %d\n", exponential_search(NULL, 7, 0));
	
	printf("\nsingle element array:\n");
	int SINGLE[] = {4};
	println_array(SINGLE, 1);
	printf("4 --> %d\n", exponential_search(SINGLE, 4, 1));
	printf("5 --> %d\n", exponential_search(SINGLE, 5, 1));
	
	printf("\neven array :\n");
	int EVEN[] = {2, 5, 7, 23, 36, 68};
	println_array(EVEN, 6);
	printf("2 --> %d\n", exponential_search(EVEN, 2, 6));
	printf("68 --> %d\n", exponential_search(EVEN, 68, 6));
	printf("7 --> %d\n", exponential_search(EVEN, 7, 6));
	printf("70 --> %d\n", exponential_search(EVEN, 70, 6));
	printf("9 --> %d\n", exponential_search(EVEN, 9, 6));
	
	printf("\nodd array :\n");
	int ODD[] = {2, 5, 7, 9, 23, 36, 68};
	println_array(ODD, 7);
	printf("2 --> %d\n", exponential_search(ODD, 2, 7));
	printf("68 --> %d\n", exponential_search(ODD, 68, 7));
	printf("7 --> %d\n", exponential_search(ODD, 7, 7));
	printf("1 --> %d\n", exponential_search(ODD, 1, 7));
	printf("22 --> %d\n", exponential_search(ODD, 22, 7));
	
	printf("\nlong array:\n (array of size 1000000 containing the first 1M even numbers)\n");
	int size = 1000000;
	int LONG[size];
	for (int i = 0; i < size; i++){
		LONG[i] = i * 2;
	}
	printf("0 --> %d\n", exponential_search(LONG, 0, size));
	printf("1999998 --> %d\n", exponential_search(LONG, (size * 2) -2 , size));
	printf("62 --> %d\n", exponential_search(LONG, 62, size));
	printf("-1 --> %d\n", exponential_search(LONG, -1, size));
	printf("2000000 --> %d\n", exponential_search(LONG, size * 2, size));
	printf("63 --> %d\n", exponential_search(LONG, 9, size));
}



////////////////////////////////////////////////////////////////////////
// implementation

int binary_search(int array[], int key, int lower, int upper, int middle) {
	return (lower > upper)?                                                                                //check bounduaries (-1 if range is empty or invalid)
			-1:
			(key < array[lower] || key > array[upper])?                                                    //check if element is out of range (return -1, element not present)
				-1:
				(array[lower] == key)?                                                                     //look for element in upper and lower position (return position if found)
					lower:
					(array[upper] == key)?
						upper:
						(array[middle] == key)?                                                            //look for element in middle position (return position if found)
							middle:
							(array[middle] > key)?
								binary_search(array, key, lower + 1, middle - 1, (lower + middle) / 2):    //recurse on upper or lower half excluding alredy checked positions
								binary_search(array, key, middle + 1, upper - 1, (middle + upper) / 2);}

int exponential_search(int array[], int key, int size) {
	if(size == 0 || array[0] > key || array[size - 1] < key) {            //check if empty or if out of range
		return -1;
	}
	
	if(size == 1) {                                                       //check if its a single element array
		return (array[0] == key)?
					0:
					-1;
	}
		
	
	int lower = 0;                                                        //set initial bounduaries
	int upper = 1;
	while(array[upper] < key) {                                           //continue until Upper is past the element
		lower = upper;
		upper = ((upper * 2) < size)?                                     //double upper (without exceding size) and->
					upper * 2:										      // ->set lower to previous upper (key was past previous upper's range)
					size - 1;
	}
	
	return binary_search(array, key, lower, upper, (lower + upper) / 2);  //binary search the range between lower and upper
}
