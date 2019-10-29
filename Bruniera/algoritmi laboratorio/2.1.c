#include <stdio.h>

int main(int argc, char** argv) {
    char str[1000];
    
    fgets(str, 1000, stdin);
    
    for(int i = 0; str[i] != 0; i++) {
        puts(&str[i]);
    }
    
    return 0;
}
