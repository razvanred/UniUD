#include<stdio.h>

int main(){
	int counter = 0;
	for(int c = getchar(); c != EOF; c = getchar()){
		if(c == '\n' || c == '\t' || c == ' ')
			counter++;
	}
	printf("\nNumero di white spaces: %d",counter);
	return 0;
}