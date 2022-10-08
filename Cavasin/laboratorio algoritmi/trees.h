#ifndef ASD_ESERCIZI_TREES_H
#define ASD_ESERCIZI_TREES_H

#include <stdbool.h>

#ifndef TREE_VALUE_SIZE
	#define TREE_VALUE_SIZE 24
#endif

typedef enum BSTRotateDirection {
	BST_ROTATE_LEFT = 0, BST_ROTATE_RIGHT = 1
} BSTRotateDirection;

typedef struct Token {
	int val;
	bool null;
} Token;

typedef struct BTNode {
	int key;
	int height;
	struct BTNode *parent, *left, *right;
	char val[TREE_VALUE_SIZE];
} BTNode;

#endif //ASD_ESERCIZI_TREES_H
