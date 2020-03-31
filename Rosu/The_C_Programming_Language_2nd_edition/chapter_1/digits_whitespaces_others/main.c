#include <stdio.h>

#define NUMBER_OF_DIGITS 10

int is_not_EOF(int c);

int is_digit(int c);

int is_white_space(int c);

void init_long_array(long array[], int length, long value);

void increment_digit_occurrences_counter(long[], int character);

int main() {

    int c;
    long white_spaces, others;
    long digits[NUMBER_OF_DIGITS];

    white_spaces = others = 0;

    init_long_array(digits, NUMBER_OF_DIGITS, 0);

    while (is_not_EOF(c = getchar())) {
        if (is_digit(c)) {
            increment_digit_occurrences_counter(digits, c);
        } else if (is_white_space(c)) {
            white_spaces++;
        } else {
            others++;
        }
    }

    printf("digits:");
    for (int i = 0; i < NUMBER_OF_DIGITS; i++)
        printf(" %ld", digits[i]);

    printf("\nwhite spaces: %ld\nothers: %ld\n", white_spaces, others);

    return 0;
}

void init_long_array(long array[], int length, long value) {
    while (--length >= 0) {
        array[length] = value;
    }
}

int is_not_EOF(const int c) {
    return c != EOF;
}

int is_digit(int c) {
    return c >= '0' && c <= '9';
}

int is_white_space(int c) {
    return c == '\t' || c == ' ' || c == '\n';
}

void increment_digit_occurrences_counter(long occurrences[], int character) {
    occurrences[character - '0']++;
}
