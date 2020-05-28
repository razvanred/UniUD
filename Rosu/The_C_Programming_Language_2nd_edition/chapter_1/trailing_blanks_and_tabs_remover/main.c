#include <stdio.h>

#define MAX_LENGTH 1000
#define UNDEFINED_BLANK_POSITION -1

int length;
char line[MAX_LENGTH];

int get_line(void);

int is_EOF(int character);

int is_not_EOF(int character);

int is_not_newline(int character);

int is_newline(int character);

int is_not_terminator(int character);

void clear_trailing_blanks();

void put_terminator(void);

int is_blank_space(int character);

int is_tab(int character);

int is_not_blank(int character);

int main() {

    while (get_line() > 0) {
        clear_trailing_blanks();
        printf("%s", line);
    }

    return 0;
}

int get_line(void) {
    int c;

    for (length = 0; length < MAX_LENGTH - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++) {
        line[length] = (char) c;
    }

    if (is_newline(c) || (is_EOF(c) && length > 0)) {
        line[length++] = '\n';
    }

    put_terminator();

    return length;
}

void clear_trailing_blanks() {
    int first_blank_position = UNDEFINED_BLANK_POSITION;
    int c;

    for (int i = 0; is_not_terminator(c = line[i]); i++) {
        if (is_not_blank(c)) {
            first_blank_position = UNDEFINED_BLANK_POSITION;
        } else if (first_blank_position == UNDEFINED_BLANK_POSITION) {
            first_blank_position = i;
        }
    }

    if (first_blank_position != UNDEFINED_BLANK_POSITION) {
        length = first_blank_position;
        put_terminator();
    }
}

void put_terminator(void) {
    line[length] = '\0';
}

int is_newline(const int c) {
    return c == '\n';
}

int is_not_newline(const int c) {
    return c != '\n';
}

int is_EOF(const int c) {
    return c == EOF;
}

int is_not_EOF(const int c) {
    return c != EOF;
}

int is_blank_space(const int c) {
    return c == ' ';
}

int is_tab(const int c) {
    return c == '\t';
}

int is_not_terminator(const int c) {
    return c != '\0';
}

int is_not_blank(const int c) {
    return !(is_tab(c) || is_blank_space(c) || is_newline(c));
}