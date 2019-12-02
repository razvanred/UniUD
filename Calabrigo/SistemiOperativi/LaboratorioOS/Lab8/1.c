/*
Scrivere un programma C che conti il numero di spazi, tab e
newline (whitespace characters) presenti nei caratteri immessi
sullo standard input.
Ripetere l’esercizio 2 della Lezione 7, definendo però una
funzione is_whitespace() per controllare se un carattere è
di spazio bianco oppure no.
*/
#include<stdio.h>

int is_whitespace(int c);

int main(){
	int c = 0, n = 0;
	while(c != EOF){
		c = getchar();
		if(is_whitespace(c)){
			n++;
		}
	}
	printf("Ci sono %d spazi\n", n);
	return 0;
}

int is_whitespace(int c){
	if(c == '\n' || c == '\t' || c == ' ')
		return 1;
	return 0;
}