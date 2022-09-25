#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "trees.h"

#define INPUT_SIZE 10000

extern void readLine(char s[], int sLength);

Token *tokenizeAlloc(int *length, char const s[]);
BTNode bTreeAlloc(Token const tokens[const]);
void bTreeDestroy(BTNode bTree);
bool isBSTree(BTNode tree);

int main(void) {
	char s[INPUT_SIZE];
	int tokensLength;

	readLine(s, INPUT_SIZE);
	Token *const tokens = tokenizeAlloc(&tokensLength, s);
	BTNode tree = bTreeAlloc(tokens);
	printf("%d\n", isBSTree(tree));

	free(tokens);
	bTreeDestroy(tree);

	return 0;
}
