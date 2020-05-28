#include <stdio.h>

int main() {
    int c;

    while ((c = getchar()) != EOF) {
        putchar(c);
    }

    printf("ending");

    return 0;
}
