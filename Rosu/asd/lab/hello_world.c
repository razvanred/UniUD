#include <stdio.h>

int f(int *x);

int main() {
    // printf("Hello world!\n") Wrong because it is inefficient
    // printf("%s\n", "Hello world!"); Better than the first
    puts("Hello world!"); // Made for print-string case

    int x, y;
    scanf("%i %i occhei", &x, &y);

    y = f(&x);

    printf("%i %i\n", x, y);

    return 0;
}

int f(int *x) {
    (*x)++; // *(x+1) va alla casella successiva di memoria. SegmentationFault se la memoria finisce
    return *x;
}