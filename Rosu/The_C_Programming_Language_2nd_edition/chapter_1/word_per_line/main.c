#include <stdio.h>

#define IN 1 /* inside word */
#define OUT 0 /* outside word */

int is_invalid_word_character(int c);

int is_EOF(int c);

int is_blank(int c);

int is_newline(int c);

int is_tab(int c);

void put_newline();

int main() {
    int c, state;

    state = OUT;

    while (!is_EOF(c = getchar())) {
        if (is_invalid_word_character(c)) {
            state = OUT;
        } else {
            if (state == OUT) {
                state = IN;
                put_newline();
            }
            putchar(c);
        }
    }

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

void put_newline() {
    putchar('\n');
}
