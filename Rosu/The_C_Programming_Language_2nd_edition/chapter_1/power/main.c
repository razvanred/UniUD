#include <stdio.h>

double positive_power(double base, double exponent);

int main() {
    printf("%.0f\n", positive_power(5, -1));
    return 0;
}

double positive_power(double base, double exponent) {
    if (exponent < 0) {
        perror("The exponent should be equal to or greater than 0");
        return -1;
    }

    if (exponent == 0) {
        return 1;
    }

    return base * positive_power(base, exponent - 1);
}
