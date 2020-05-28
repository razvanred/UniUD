#include <stdio.h>

#define NUMBER_OF_CHARACTERS 128

int is_not_EOF(int c);

void print_characters_histogram(int characters[]);

void print_character_bar(int character, int length);

int main() {
    int c;

    int characters_lengths[NUMBER_OF_CHARACTERS];

    for (int i = 0; i < NUMBER_OF_CHARACTERS; i++)
        characters_lengths[i] = 0;

    while (is_not_EOF(c = getchar())) {
        characters_lengths[c]++;
    }

    print_characters_histogram(characters_lengths);

    return 0;
}

int is_not_EOF(int c) {
    return c != EOF;
}

void print_characters_histogram(int characters[]) {
    for (int i = 0; i < NUMBER_OF_CHARACTERS; i++) {
        print_character_bar(i, characters[i]);
    }
}

void print_character_bar(int character, int length) {
    printf("%c: ", character);

    for (int i = 0; i < length; i++)
        putchar('*');

    putchar('\n');
}
