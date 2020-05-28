#include <stdio.h>
#include <string.h>

float from_celsius_to_fahrenheit(int celsius);

main() {
    float fahrenheit;
    int celsius;

    const int step = 20;
    const int upper = 220;
    const int lower = 0;

    const char* fahrenheit_title = "Fahrenheit";
    const char* celsius_title = "Celsius";

    const unsigned int fahrenheit_title_length = strlen(fahrenheit_title);
    const unsigned int celsius_title_length = strlen(celsius_title);

    printf("%*s\t%*s\n", celsius_title_length, celsius_title, fahrenheit_title_length, fahrenheit_title);

    for(celsius = lower; celsius <= upper; celsius += step){
        fahrenheit = from_celsius_to_fahrenheit(celsius);
        printf("%*d\t%*.1f\n", celsius_title_length, celsius, fahrenheit_title_length, fahrenheit);
    }
}

float from_celsius_to_fahrenheit(const int celsius) {
    return (9.f / 5.f) * (float) celsius + 32;
}
