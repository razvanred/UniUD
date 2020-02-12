#include<stdio.h>

int main(){
    //lettura di n numeri interi da std input, e inserimento dei numeri in un vettore
    int counter=0, size = 0;
    int vec[size];
    printf("Scrivi quanti numeri vuoi inserire:\n");
    scanf("%d", &size);
    for(int i=0;i<size;i++){
        scanf("%d", &vec[i]);
        counter+=vec[i];
    }
    printf("Il totale e': %d", counter);
}