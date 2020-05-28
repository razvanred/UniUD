#include <stdio.h>

int main() {
    int last = ' ';
    int actual;

    while((actual = getchar()) != EOF) {
        if(last != ' ' || actual != ' ') {
            last = actual;
            putchar(actual);
        }
    }

    return 0;
}
