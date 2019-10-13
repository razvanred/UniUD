#include <stdio.h>

int merge(int A[], int p, int q, int r){ //p: indice prima cella primo vettore, q: indice ultima cella secondo vettore, r: indice ultima cella prmo vettores=
	int s = q - p + 1;
	int B[s];
	int i = p, j = r + 1;
	for(int k = 0; k < s; k++){
		if(i > r){                //se il primo vettore è finito prendo solo dal secondo
			B[k] = A[j];
			j++;
		} else if(j > q) {        //se il secondo vettore è finito prendo solo dal primo
			B[k] = A[i];
			i++;
		} else if(A[i] <= A[j]){  //il numero minore va per primo in B, se sono uguali va prima quello nel primo vettore (per stabilità)
			B[k] = A[i];
			i++;
		} else {
			B[k] = A[j];
			j++;
		}
	}
	
	for(int k = 0; k < s; k++){   //ripopolo A con gli elementi ordinati
		A[p+k] = B[k];
	}
}

int merge_sort(int A[], int p, int q){ // p: indice prima cella, q: indice ultima cella (non dimensione)
	if(q > p){                    // se non è banale
		int r = (p + q) / 2;
		merge_sort(A, p, r);      //ordino le due metà
		merge_sort(A, r + 1, q);
		merge(A, p, q, r);        //unisco le due metà
	}
}




//////////////////////////////////////////////////////////////
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
	merge_sort(NULL, 0, -1);  // non ha senso
	println_array(NULL, 0);
	
	printf("\nsingle element array:\n");
	int SINGLE[] = {4};
	println_array(SINGLE, 1);
	merge_sort(SINGLE, 0, 0);
	println_array(SINGLE, 1);
	
	printf("\nordered array :\n");
	int ORDER[] = {2, 5, 7, 9, 23, 36, 68};
	println_array(ORDER, 7);
	merge_sort(ORDER, 0, 6);
	println_array(ORDER, 7);
	
	printf("\nreverse array :\n");
	int REVERSE[] = {68, 36, 23, 9, 7, 5, 2};
	println_array(REVERSE, 7);
	merge_sort(REVERSE, 0, 6);
	println_array(REVERSE, 7);
	
	printf("\nrandom array :\n");
	int RANDOM[] = {43, 234, 87, 2, 8, 34, 12, 3, 54, 23, 76, 12, 89, 84};
	println_array(RANDOM, 14);
	merge_sort(RANDOM, 0, 13);
	println_array(RANDOM, 14);
}