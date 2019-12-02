/*
Scrivere un programma C che conti il numero di parole
immesse sullo standard input (si considerino come delimitatori
di parola i whitespace characters).
*/
#include<stdio.h>

int main(){
	int c = 0, n = 0, wc = 0;
	while(c != EOF){
		c = getchar();
		if(c == ' ' || c == '\n' || c == '\t'){
			if(n != 0){
				wc++;
				n = 0;
			}
		}else{
			n++;
		}
	}
	printf("Hai  scritto: %d", wc);
}