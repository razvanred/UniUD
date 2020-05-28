#include <stdio.h>

int is_newline(int c);

int is_blank(int c);

int is_tab(int c);

int is_EOF(int c);

int main() {
    int c;
    long newlines = 0;
    long tabs = 0;
    long blanks = 0;

    while (!is_EOF((c = getchar()))) {
        if (is_blank(c)) {
            blanks++;
        } else if (is_tab(c)) {
            tabs++;
        } else if (is_newline(c)) {
            newlines++;
        }
    }

    printf("number of lines: %ld\n", newlines);
    printf("number of tabs: %ld\n", tabs);
    printf("number of blanks: %ld\n", blanks);

    return 0;
}

int is_newline(int c) {
    return c == '\n';
}

int is_blank(int c) {
    return c == ' ';
}

int is_tab(int c) {
    return c == '\t';
}

int is_EOF(int c) {
    return c == EOF;
}
