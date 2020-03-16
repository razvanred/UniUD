#include <stdio.h> // include information about standard library
#include <string.h>

/*
 * print Fahrenheit-Celsius table
 * for fahr = 0. 20, ..., 220
 */
float from_fahrenheit_to_celsius(int fahrenheit);

main() { // define a function named main that receives no argument values
    float celsius;
    int fahrenheit;

    const int lower = 0;
    const int upper = 220;
    const int step = 20;

    fahrenheit = lower;

    const char* fahrenheit_title = "Fahrenheit";
    const char* celsius_title = "Celsius";

    const unsigned int fahrenheit_header_length = strlen(fahrenheit_title);
    const unsigned celsius_header_length = strlen(celsius_title);

    printf("%*s\t%*s\n", fahrenheit_header_length, fahrenheit_title, celsius_header_length, celsius_title);

    while(fahrenheit <= upper) {
        celsius = from_fahrenheit_to_celsius(fahrenheit);
        printf("%*d\t%*.1f\n", fahrenheit_header_length, fahrenheit, celsius_header_length, celsius);
        fahrenheit += step;
    }
}

float from_fahrenheit_to_celsius(int fahrenheit) {
    return (5.0f / 9.0f) * (float) (fahrenheit - 32);
}