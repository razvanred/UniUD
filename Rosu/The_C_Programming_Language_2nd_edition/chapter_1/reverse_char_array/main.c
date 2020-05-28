#include <stdio.h>

#define MAX_LENGTH 1000

int get_line(char output[], int max_length);

int is_EOF(int character);

int is_not_EOF(int character);

int is_newline(int character);

int is_not_newline(int character);

int is_not_terminator(int character);

void put_terminator(char *to, int length);

void reverse(char s[], int length);

void copy(char output[], const char input[]);

int main() {

    char current_line[MAX_LENGTH];
    int current_length;

    while ((current_length = get_line(current_line, MAX_LENGTH)) > 0) {
        reverse(current_line, current_length);
        printf("%s\n", current_line);
    }

    return 0;
}

int get_line(char output[], int max_length) {
    int length, c;

    for (length = 0; length < max_length - 1 && is_not_newline(c = getchar()) && is_not_EOF(c); length++)
        output[length] = (char) c;

    put_terminator(output, length);

    return length;
}

void reverse(char s[], int length) {
    char temp[MAX_LENGTH];
    copy(temp, s);
    for (int i = 0; i < length; i++) {
        s[i] = temp[length - i - 1];
    }
}

void copy(char output[], const char input[]) {
    for (int i = 0; is_not_terminator(output[i] = input[i]); i++);
}

int is_EOF(int c) {
    return c == EOF;
}

int is_not_EOF(int c) {
    return c != EOF;
}

int is_newline(int c) {
    return c == '\n';
}

int is_not_newline(int c) {
    return c != '\n';
}

int is_not_terminator(int c) {
    return c != '\0';
}

void put_terminator(char to[], int length) {
    to[length] = '\0';
}
