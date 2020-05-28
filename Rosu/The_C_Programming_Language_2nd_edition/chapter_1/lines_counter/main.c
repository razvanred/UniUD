#include <stdio.h>

int main() {
    long i = 0;
    int c;

    while((c = getchar()) != EOF) {
        if(c == '\n') {
            i++;
        }
    }

    printf("lines count: %ld\n", i);

    return 0;
}
