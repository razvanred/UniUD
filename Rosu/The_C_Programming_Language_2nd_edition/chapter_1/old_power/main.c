#include <stdio.h>

int power(); // no parameter list was permitted, so the compiler could not readily check that power was being called correctly

int main() {
    printf("%d\n", power(5, 4));
    return 0;
}

power(base, exponent)
int base, exponent;
{
    if(exponent < 0) {
        perror("The exponent should be equal to or greater than 0");
        return -1;
    }

    if(exponent == 0) {
        return 1;
    }

    return base * power(base, exponent - 1);
}
