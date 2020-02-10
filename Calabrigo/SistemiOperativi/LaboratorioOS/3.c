/*
Scrivere un programma che legga una sequenza di numeri dallo standard
input, e:
I Se l’utente ha passato l’opzione -r sulla riga di comando,
stampi i numeri in ordine inverso.
I Se l’utente ha passato l’opzione -s, stampi i numeri ordinati in
senso crescente.
I Se l’utente ha passato l’opzione -S, stampi i numeri ordinati in
senso decrescente.
I Se l’utente non ha passato alcuna opzione, stampare un
messaggio di errore e non fare nulla (ancora prima di leggere
alcunché)
*/
#include<stdio.h>
#include<string.h>

void readIntArray(int *vec, int size);
void insertionSort(int *vec, int size);
void insertionSortReverse(int *vec, int size);
void reverse(int *vec, int size);
void swap(int *vec, int i, int j);
void printIntArray(int *vec, int size);

int main(int argc, char **argv){
    int size = 0;
    printf("Scrivi la size: \n");
    scanf("%d",&size);

    int vec[size];
    readIntArray(vec, size);
    if(strcmp(argv[1], "-r")){
        reverse(vec, size);
    }else if(strcmp(argv[1], "-s")){
        insertionSort(vec, size);
    }else if(strcmp(argv[1], "-S")){
        insertionSortReverse(vec, size);
    }
    printIntArray(vec, size);
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

void insertionSortReverse(int *vec, int size){
	for(int i = 0;i < size; i++){
		for(int j=i;j < size; j++){
			if(vec[i] < vec[j]){
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

void readIntArray(int *vec, int size){
    for(int i=0;i<size;i++){
        scanf("%d", &vec[i]);
    }
}

void reverse(int *vec, int size){
    for(int i=0;i<size/2;i++){
        swap(vec, i, size-1-i);
    }
}

void printIntArray(int *vec, int size){
    for(int i=0;i<size;i++){
        printf("%d\t", vec[i]);
    }
}