#include <stdio.h>

#define MAX_LINE_SIZE 1000

int m_getline(char line[], int max_line_size);

void copy(char to[], const char from[]);

int is_not_EOF(int character);

int is_not_newline(int character);

int is_newline(int character);

int is_terminator(int character);

void put_terminator(char to[], int length);

int main(void) {
    int current_length;
    int max_length;
    char current_line[MAX_LINE_SIZE];
    char longest_line[MAX_LINE_SIZE];

    max_length = 0;

    while ((current_length = m_getline(current_line, MAX_LINE_SIZE)) > 0) {
        if (current_length > max_length) {
            max_length = current_length;
            copy(longest_line, current_line);
        }
    }

    if (max_length > 0) {
        printf("%s", longest_line);
    }

    return 0;
}

int m_getline(char line[], int max_line_size) {
    int c, length;

    for (length = 0; length < max_line_size - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++) {
        line[length] = (char) c;
    }

    if (is_newline(c)) {
        line[length++] = (char) c;
    }

    put_terminator(line, length);

    return length;
}

void put_terminator(char to[], const int length) {
    to[length] = '\0';
}

int is_not_newline(const int c) {
    return c != '\n';
}

int is_newline(const int c) {
    return c == '\n';
}

int is_not_EOF(const int c) {
    return c != EOF;
}

void copy(char to[], const char from[]) {
    for (int i = 0; !is_terminator(to[i] = from[i]); i++);
}

int is_terminator(const int c) {
    return c == '\0';
}