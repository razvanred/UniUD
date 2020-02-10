#include <stdio.h>
#include <string.h>

int factorial(int n);
int sum(int vec[], int size);
void swap(int *vec, int i, int j);
void insertionSort(int *vec, int size);
void toString(int *vec, int size);

int main()
{
	int vec[] = {7,4,8,3,12,2};
	insertionSort(vec, 6);
	toString(vec, 6);
return 0;
}

void insertionSort(int *vec, int size){
	for(int i = 0;i < size; i++){
		for(int j=i;j < size; j++){
			if(vec[i] > vec[j]){
				swap(vec, i, j);
			}
		}
	}

}

void swap(int *vec, int i, int j){
	int temp = vec[i];
	vec[i] = vec[j];
	vec[j] = temp;
}

void toString(int *vec, int size){
	for(int i=0;i<size;i++){
		printf("%d\n",vec[i]);
	}
}

int factorial(int n){
	if(n==0){
		return 1;
	}else{
		return n*factorial(n-1);
	}
}

int sum(int vec[], int size){
	int acc=0;
	for(int i=0;i<size;i++){
		acc+=vec[i];
	}
	return acc;
}
