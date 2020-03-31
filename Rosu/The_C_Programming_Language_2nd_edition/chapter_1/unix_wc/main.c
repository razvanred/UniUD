#include <stdio.h>

#define IN 1 /* inside word */
#define OUT 0 /* outside word */

int is_invalid_word_character(int c);

int is_EOF(int c);

int is_blank(int c);

int is_newline(int c);

int is_tab(int c);

int main() {
    int c, state;
    long lines, words, characters;

    state = OUT;

    lines = words = characters = 0;

    while(!(is_EOF(c = getchar()))) {
        characters++;

        if(is_newline(c)) {
            lines++;
        }

        if(is_invalid_word_character(c)) {
            state = OUT;
        } else if (state == OUT) {
            state = IN;
            words++;
        }
    }

    printf("%ld %ld %ld\n", lines, words, characters);

    return 0;
}

int is_invalid_word_character(int c) {
    return is_newline(c) || is_tab(c) || is_blank(c);
}

int is_EOF(int c) {
    return c == EOF;
}

int is_blank(int c) {
    return c == ' ';
}

int is_newline(int c) {
    return c == '\n';
}

int is_tab(int c) {
    return c == '\t';
}