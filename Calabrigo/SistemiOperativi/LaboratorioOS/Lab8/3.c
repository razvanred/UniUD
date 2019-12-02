/*
Scrivere un programma che ordini un array di numeri interi,
utilizzando un algoritmo di ordinamento visto a lezione di
Algoritmi e Strutture Dati (es. bubble sort o insertion sort).
*/
#include<stdio.h>
void insertionSort(int arr[], int dim);
void printArray(int arr[], int dim);

int main(){
	int array[] = {10,9,4,12,3,3,2};
	int dim = 7;
	insertionSort(array, dim);
	printArray(array,dim);
	return 0;
}

/* Function to sort an array using insertion sort*/
void insertionSort(int arr[], int n)  
{  
    int i, key, j;  
    for (i = 1; i < n; i++) 
    {  
        key = arr[i];  
        j = i - 1;  
  
        /* Move elements of arr[0..i-1], that are  
        greater than key, to one position ahead  
        of their current position */
        while (j >= 0 && arr[j] > key) 
        {  
            arr[j + 1] = arr[j];  
            j = j - 1;  
        }  
        arr[j + 1] = key;  
    }
} 

void printArray(int arr[], int dim){
	for(int i = 0; i < dim; i++){
		printf("\n %d", arr[i]);
	}
}