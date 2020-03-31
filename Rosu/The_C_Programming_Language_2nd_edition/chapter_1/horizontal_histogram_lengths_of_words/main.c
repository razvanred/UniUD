#include <stdio.h>

int is_not_EOF(int c);

int is_whitespace(int c);

void print_horizontal_bar(int length);

int main() {
    int c;
    int length = 0;

    while (is_not_EOF(c = getchar())) {
        if (!is_whitespace(c)) {
            length++;
        } else {
            print_horizontal_bar(length);
            length = 0;
        }
    }

    return 0;
}

int is_not_EOF(const int c) {
    return c != EOF;
}

int is_whitespace(const int c) {
    return c == '\n' || c == '\t' || c == ' ';
}

void print_horizontal_bar(const int length) {
    if (length > 0) {
        for (int i = 0; i < length; i++)
            putchar('*');

        putchar('\n');
    }
}
