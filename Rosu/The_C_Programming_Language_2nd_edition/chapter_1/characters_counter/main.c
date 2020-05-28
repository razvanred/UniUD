#include <stdio.h>

int main() {
    long i;

    for (i = 0; getchar() != EOF; i++);

    printf("characters inserted: %ld\n", i);

    return 0;
}
