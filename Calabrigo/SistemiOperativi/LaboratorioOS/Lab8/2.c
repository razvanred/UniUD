/*
Definire una funzione int lg(int n) che trovi il massimo
numero m tale che 10^m ≤ n (ovvero la parte intera di log10 n).
*/
#include<stdio.h>
int lg(int n);

int main(){
	printf("log in base 10 di 10023 è %d", lg(10023));
	return 0;
}

int lg(int n){
	int m = 10, c = 0;
	while(m <= n){
		m *= 10;
		c++;
	}
	return c;
}