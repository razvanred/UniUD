#include <stdio.h>

#define MAX_LENGTH 1000

int length_longest_line;
char current_line[MAX_LENGTH];
char longest_line[MAX_LENGTH];

int get_line(void);

void copy(void);

void put_terminator(char to[], int length);

int is_newline(int character);

int is_EOF(int character);

int is_not_newline(int character);

int is_not_EOF(int character);

int is_not_terminator(int character);

int main() {
    int length_current_line;

    extern int length_longest_line; // redundant
    extern char longest_line[]; // redundant

    while ((length_current_line = get_line()) > 0) {
        if (length_current_line > length_longest_line) {
            length_longest_line = length_current_line;
            copy();
        }
    }

    if (length_longest_line > 0) {
        printf("%s\n", longest_line);
    }

    return 0;
}

int get_line(void) {
    int c, length;
    extern char current_line[]; // redundant

    for (length = 0; length < MAX_LENGTH - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++)
        current_line[length] = (char) c;

    if (is_newline(current_line[length])) {
        current_line[length++] = (char) c;
    } else if (is_EOF(current_line[length]) && length > 0) {
        current_line[length++] = '\n';
    }

    put_terminator(current_line, length);

    return length;
}

void put_terminator(char to[], const int length) {
    to[length] = '\0';
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

int is_not_terminator(const int c) {
    return c != '\0';
}

void copy(void) {
    extern char longest_line[], current_line[]; // redundant
    for (int i = 0; is_not_terminator(longest_line[i] = current_line[i]); i++);
}

