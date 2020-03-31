#include <stdio.h>

#define MAX_SIZE 20

#define TRUE 1
#define FALSE 0

int m_getline(char output[], int max_size);

void copy(char to[], const char from[]);

void put_terminator(char to[], int length);

int is_not_newline(int character);

int is_newline(int character);

int is_not_terminator(int character);

int is_not_EOF(int character);

int is_EOF(int character);

int main(void) {
    int length_current_line, length_longest_line, length_previous_line;

    char current_line[MAX_SIZE];
    char longest_line[MAX_SIZE];
    char temp_line[MAX_SIZE];

    int get_more = FALSE;
    length_previous_line = length_longest_line = 0;

    while ((length_current_line = m_getline(current_line, MAX_SIZE)) > 0) {
        if (is_not_newline(current_line[length_current_line - 1])) {
            if (!get_more) {
                copy(temp_line, current_line);
            }
            length_previous_line += length_current_line;
            if (length_longest_line <= length_previous_line) {
                length_longest_line = length_previous_line;
            }
            get_more = TRUE;
        } else {
            if (get_more) {
                if (length_longest_line < length_previous_line + length_current_line) {
                    length_longest_line = length_current_line + length_previous_line;
                    copy(longest_line, temp_line);
                    longest_line[MAX_SIZE - 2] = '\n';
                }
                get_more = FALSE;
            } else if (length_longest_line < length_current_line) {
                length_longest_line = length_current_line;
                copy(longest_line, current_line);
            }
            length_previous_line = 0;
        }
    }

    if (length_longest_line > 0) {
        printf("%s", longest_line);
        printf("length: %d\n", length_longest_line);
    }

    return 0;
}

int m_getline(char output[], const int max_size) {
    int c, length;

    for (length = 0; length < max_size - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++) {
        output[length] = (char) c;
    }

    if (is_newline(c)) {
        output[length++] = (char) c;
    } else if (is_EOF(c) && length > 0) {
        output[length++] = '\n';
    }

    put_terminator(output, length);

    return length;
}

void copy(char to[], const char from[]) {
    for (int i = 0; is_not_terminator(to[i] = from[i]); i++);
}

int is_not_newline(const int c) {
    return c != '\n';
}

int is_newline(const int c) {
    return c == '\n';
}

int is_not_terminator(const int c) {
    return c != '\0';
}

int is_not_EOF(int character) {
    return character != EOF;
}

int is_EOF(const int c) {
    return c == EOF;
}

void put_terminator(char to[], const int length) {
    to[length] = '\0';
}