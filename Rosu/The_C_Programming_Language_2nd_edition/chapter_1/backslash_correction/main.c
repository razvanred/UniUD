#include <stdio.h>

int is_EOF(int c);

int is_backslash(int c);

int is_tab(int c);

int is_backspace(int c);

void put_backslash();

void put_backslash_representation();

void put_backspace_representation();

void put_tab_representation();

void putchar_custom(int c);

int main() {
    int actual;

    while (!is_EOF(actual = getchar())) {
        putchar_custom(actual);
    }

    return 0;
}

void putchar_custom(int c) {
    if (is_backslash(c)) {
        put_backslash_representation();
    } else if (is_backspace(c)) {
        put_backspace_representation();
    } else if (is_tab(c)) {
        put_tab_representation();
    } else {
        putchar(c);
    }
}

void put_backslash_representation() {
    int number_of_backslashes = 2;
    for (int i = 0; i < number_of_backslashes; i++) {
        put_backslash();
    }
}

void put_backspace_representation() {
    put_backslash();
    putchar('b');
}

void put_tab_representation() {
    put_backslash();
    putchar('t');
}

int is_EOF(int c) {
    return c == EOF;
}

int is_backslash(int c) {
    return c == '\\';
}

int is_backspace(int c) {
    return c == '\b';
}

int is_tab(int c) {
    return c == '\t';
}

void put_backslash() {
    putchar('\\');
}