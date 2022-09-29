#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "trees.h"

#define INPUT_SIZE 10000

extern void readLine(char s[], int sLength);
extern Token *tokenizeAlloc(int *length, char const s[]);
extern BTNode *bTreeAlloc(struct Token const tokens[]);
extern void bTreeFree(BTNode **root);
extern bool isBSTree(BTNode *root);

int main(void) {
	char s[INPUT_SIZE];
	int tokensLength;

	readLine(s, INPUT_SIZE);
	Token *const tokens = tokenizeAlloc(&tokensLength, s);
	BTNode *tree = bTreeAlloc(tokens);
	printf("%d\n", isBSTree(tree));

	free(tokens);
	bTreeFree(&tree);

	return 0;
}
