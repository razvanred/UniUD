#include <stdio.h>

void editArray(int arrc, int arrv[]);
void printArray(int arrc, int arrv[]);

int main() {
    int a[100];
    char s[6] = {'h', 'e', 'l', 'l', 'o', 0};
    char s1[] = "hello";

    // quando leggo le stringhe in input non mettere l'&

    puts(s);

    *a = 7;
    *(a + 1) = 8;
    *(a + 2) = 9;
    
    editArray(3, a);
    printArray(3, a);

    return 0;
}

void editArray(int arrc, int* arrv) {
    for(int i = 0; i < arrc; i++) {
        (*(arrv + i))++;
    }
}

void printArray(int arrc, int* arrv) {
    printf("%s", "[");
    for(int i = 0; i < arrc; i++) {
        printf("%i", *(arrv + i));
        if(i + 1 != arrc) {
            printf("%s", ", ");
        }
    }
    puts("]");
}