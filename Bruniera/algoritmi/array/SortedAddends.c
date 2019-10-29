#include <stdio.h>
#include <stdbool.h>

//////////////////////////////////////////////////////////////////
// declaration

bool search_addends(int array[], int size, int k);



//////////////////////////////////////////////////////////////////
// implementation

bool search_addends(int array[], int size, int k) {
	int i = 0;
	int j = size - 1;
	
	while(i < j) {
		int sum = array[i] + array[j];
		
		if(sum == k) {
			return true;
		} else if(sum > k) {
			j--;
		} else {
			i++;
		}
	}
	
	return false;
}



//////////////////////////////////////////////////////////////////
// test

int main(int argc, char** argv) {
	int array[] = {1, 4, 6, 9, 34, 46, 67, 100};
	
	puts((search_addends(array, 8, 55))?
		"yes":
		"no");
	
	puts((search_addends(array, 8, 54))?
		"yes":
		"no");
}
