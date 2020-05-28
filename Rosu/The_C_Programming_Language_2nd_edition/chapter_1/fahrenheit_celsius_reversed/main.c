#include <stdio.h>

#define LOWER 0 /* lower limit of table */
#define UPPER 220 /* upper limit */
#define STEP 20 /* step size */

float from_fahrenheit_to_celsius(int fahrenheit);

void print_temperature(int fahrenheit, float celsius);

int main() {

    for (int fahrenheit = UPPER; fahrenheit >= LOWER; fahrenheit -= STEP) {
        print_temperature(fahrenheit, from_fahrenheit_to_celsius(fahrenheit));
    }

    return 0;
}

float from_fahrenheit_to_celsius(int fahrenheit) {
    return (5.f / 9.f) * (float) (fahrenheit - 32);
}

void print_temperature(int fahrenheit, float celsius) {
    printf("%3d\t%6.1f\n", fahrenheit, celsius);
}