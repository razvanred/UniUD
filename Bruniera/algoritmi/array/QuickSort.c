#include <stdio.h>

////////////////////////////////////////////////////////////////////////
// declaration

void swap(int array[], int first, int second);                      //scambia gli elementi array[first] e array[second]
int find_me(int array[], int lower, int upper);                     //seleziona l'elemento mediano; complessità theta(n)
int partition(int array[], int lower, int upper, int pivot);        //accumula a sinistra ed a destra i valori minori-uguali e maggiori del pivot. restituisce la posizione del pivot al limitare delle due metà vettore
void quick_sort(int array[], int lower, int upper);                 //ordina i valori del vettore array[] tra le posizioni upper e lower
int linear_search(int array[], int lower, int upper, int element);  //cerca la posizione di un elemento tra le posizioni lower e upper
int cubic_selection(int array[], int lower, int upper, int index);  //usa selection sort per trovare l'index-esimo elemento in ordine di dimensioni; complessita theta(n^2)



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
	quick_sort(array, 0, 10);
	println_array(array, 11);
	
	printf("\n");
	
	int array1[] = {1, 3, 0, 5, 7, 2, 9, 8, 6, 4, 6, 10, 12, 65, 43, 67, 78, 23, 56, 89, 52, 46, 18, 30, 40, 72, 40};
	
	println_array(array1, 27);
	quick_sort(array1, 0, 26);
	println_array(array1, 27);
	
	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

int cubic_selection(int array[], int lower, int upper, int index) {
	int selected;// = lower;
	
	for(int i = lower; i <= index; i++) {
		selected = i;
		for(int j = i + 1; j <= upper; j++) {
			if(array[j] < array[selected]) {
				selected = j;
			}
		}
		swap(array, selected, i);
	}
	
	return array[index];
}

void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

int find_me(int array[], int lower, int upper) {
	int median;

	if((upper - lower) > 4) {
		int len = ((upper - lower + 1) / 5) +
				((((upper - lower) % 5) > 0)?
					1:
					0);
		int spare[len];
		
		int iter = 0;
		int current = lower;
		while(current <= upper) {
			int next = ((current + 4) > upper)?
							upper:
							current + 4;
			
			spare[iter] = cubic_selection(array, current, next, (current + next) / 2);
			iter++;
			current = next + 1;
		}
		
		median = find_me(spare, 0, len - 1);
	} else {
		median = cubic_selection(array, lower, upper, (lower + upper) / 2);
	}
	
	return median;
}

int linear_search(int array[], int lower, int upper, int element) {
	int i = -1;
	for(i = upper; (i >= lower) && (array[i] != element); i--);
	return i;
}

int partition(int array[], int lower, int upper, int pivot) {
	swap(array, upper, linear_search(array, lower, upper, pivot));
	
	int break_even = lower - 1;
	for(int i = lower; i <= upper; i++){
		if(array[i] <= pivot){
			break_even++;
			swap(array, break_even, i);
		}
	}	
	
	return break_even;
}

void quick_sort(int array[], int lower, int upper) {
	if(upper > lower) {
		int break_even = partition(array, lower, upper, find_me(array, lower, upper));
		quick_sort(array, lower, break_even - 1);
		quick_sort(array, break_even + 1, upper);
	}
}
