#include <stdio.h>

#define MAX_SIZE 1000
#define MIN_PRINT_SIZE 80

int get_line(char output[], int max_length);

int is_not_EOF(int character);

int is_not_newline(int character);

int is_newline(int character);

int is_EOF(int character);

void put_terminator(char to[], int length);

int main() {
    char current_line[MAX_SIZE];
    int current_line_length;

    while ((current_line_length = get_line(current_line, MAX_SIZE)) > 0) {
        if (current_line_length >= MIN_PRINT_SIZE)
            printf("%s", current_line);
    }

    return 0;
}

int get_line(char output[], const int max_length) {
    int c;
    int length;

    for (length = 0; length < max_length - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++)
        output[length] = (char) c;

    if (is_newline(c)) {
        output[length++] = (char) c;
    } else if (is_EOF(c) && length > 0) {
        output[length++] = '\n';
    }

    put_terminator(output, length);

    return length;
}

int is_not_EOF(int c) {
    return c != EOF;
}

int is_not_newline(int c) {
    return c != '\n';
}

int is_newline(int c) {
    return c == '\n';
}

int is_EOF(int c) {
    return c == '\n';
}

void put_terminator(char to[], int length) {
    to[length] = '\0';
}