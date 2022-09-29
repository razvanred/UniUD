#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "trees.h"

/*
 * This implementation prioritizes stratified implementation over minor performance optimizations.
 * AVL operations are implemented on top of BST operations, which are in turn implemented on top of binary tree operations.
 * For example, the rotation function can be used independently as a BST operation and handles heights correctly.
 */

extern void swap(int *a, int *b);
extern int max(int a, int b);

Token *tokenizeAlloc(int *const length, char const s[]) {
	assert(s);

	*length = 0;
	for(int pc = ' ', i = 0; s[i] != 0; ++i) {
		if(isspace(pc) && !isspace(s[i])) {
			++*length;
		}
		pc = (unsigned char)s[i];
	}
	Token *const tokens = malloc((unsigned)*length*sizeof(Token));
	for(int i = 0; i < *length; ++i) {
		for(; isspace(*s); ++s);
		if(strncmp(s, "NULL", 4) == 0) {
			tokens[i].null = true;
			s += 4;
		} else {
			int n;
			sscanf(s, "%d%n", &tokens[i].val, &n); // NOLINT(cert-err34-c)
			tokens[i].null = false;
			s += n;
		}
	}
	return tokens;
}

BTNode *bTNodeAlloc(int const key, char const val[const]) {
	assert(val);

	BTNode *const node = malloc(sizeof(BTNode));
	node->key = key;
	strncpy(node->val, val, TREE_VALUE_SIZE);
	node->parent = NULL;
	node->left = NULL;
	node->right = NULL;
	node->height = 1;
	return node;
}

static BTNode *bTreeAllocHelper(BTNode *const parent, int *const i, Token const tokens[const]) {
	assert(i);
	assert(tokens);

	if(tokens[*i].null) {
		++(*i);
		return NULL;
	} else {
		BTNode *const t = bTNodeAlloc(tokens[(*i)++].val, "");
		t->parent = parent;
		t->left = bTreeAllocHelper(t, i, tokens);
		if(t->left) {
			t->height = t->left->height+1;
		}
		t->right = bTreeAllocHelper(t, i, tokens);
		if(t->right && t->height < t->right->height+1) {
			t->height = t->right->height+1;
		}
		return t;
	}
}

static void swapNodeContent(BTNode *const a, BTNode *const b) {
	assert(a);
	assert(b);

	char tv[TREE_VALUE_SIZE];
	strcpy(tv, a->val);
	strcpy(a->val, b->val);
	strcpy(b->val, tv);
	swap(&a->key, &b->key);
}

int bTreeHeight(BTNode const *const root) {
	if(root == NULL) {
		return 0;
	}
	return root->height;
}

static void bTreeHeightFixUp(BTNode *cur) {
	while(cur != NULL) {
		int height = 1+max(bTreeHeight(cur->left), bTreeHeight(cur->right));
		cur->height = height;
		cur = cur->parent;
	}
}

BTNode *bTreeAlloc(struct Token const tokens[]) {
	assert(tokens);

	int i = 0;
	return bTreeAllocHelper(NULL, &i, tokens);
}

void bTreeFree(BTNode **const root) {
	assert(root);

	if(*root == NULL) {
		return;
	}
	bTreeFree(&(*root)->left);
	bTreeFree(&(*root)->right);
	free(*root);
	*root = NULL;
}

static void bTreePrePrintHelper(BTNode const *const cur, void (*const print)(BTNode const *)) {
	assert(print);

	print(cur);
	if(cur == NULL) {
		return;
	}
	bTreePrePrintHelper(cur->left, print);
	bTreePrePrintHelper(cur->right, print);
}

void bTreePrePrint(BTNode const *const root, void (*const print)(BTNode const *)) {
	assert(print);

	bTreePrePrintHelper(root, print);
	puts("");
}

static void bSTRotate(BTNode **const cur, BSTRotateDirection const direction) {
	assert(cur && *cur);

	BTNode *const right = (*cur)->right;
	BTNode *const left = (*cur)->left;
	if(direction == BST_ROTATE_LEFT) {
		assert(right);
		BTNode *const rightLeft = (*cur)->right->left;

		right->parent = (*cur)->parent;

		(*cur)->parent = right;
		right->left = *cur;
		if(rightLeft) {
			rightLeft->parent = *cur;
		}
		(*cur)->right = rightLeft;

		*cur = right;

		bTreeHeightFixUp((*cur)->left);
	} else {
		assert(left);
		BTNode *const leftRight = (*cur)->left->right;

		left->parent = (*cur)->parent;

		(*cur)->parent = left;
		left->right = *cur;
		if(leftRight) {
			leftRight->parent = *cur;
		}
		(*cur)->left = leftRight;

		*cur = left;

		bTreeHeightFixUp((*cur)->right);
	}
}

static BTNode *bSTreeMin(BTNode *cur) {
	if(cur == NULL) {
		return NULL;
	}
	while(cur->left) {
		cur = cur->left;
	}
	return cur;
}

BTNode *bSTFind(BTNode *cur, int const key) {
	while(cur != NULL && cur->key != key) {
		if(key < cur->key) {
			cur = cur->left;
		} else {
			cur = cur->right;
		}
	}
	return cur;
}

BTNode *bSTSuccessor(BTNode const *cur) {
	if(cur == NULL) {
		return NULL;
	}
	if(cur->right) {
		return bSTreeMin(cur->right);
	}
	BTNode *parent = cur->parent;
	while(parent && parent->right == cur) {
		cur = cur->parent;
		parent = parent->parent;
	}
	return parent;
}

bool isBSTree(BTNode *const root) {
	BTNode *pre = bSTreeMin(root);
	if(pre == NULL) {
		return true;
	}
	BTNode *cur = bSTSuccessor(pre);
	if(pre == NULL) {
		return true;
	}
	while(cur != NULL) {
		if(pre->key >= cur->key) {
			return false;
		}
		pre = cur;
		cur = bSTSuccessor(cur);
	}
	return true;
}

void bSTreeAdd(BTNode **cur, BTNode *const node) {
	assert(cur);
	assert(node);

	BTNode *pre = NULL;
	while(*cur != NULL) {
		pre = *cur;
		if(node->key < (*cur)->key) {
			cur = &(*cur)->left;
		} else {
			cur = &(*cur)->right;
		}
	}
	node->parent = pre;
	*cur = node;
	bTreeHeightFixUp(pre);
}

BTNode *bSTreeRemove(BTNode **const root, BTNode *const node) {
	assert(root && *root);
	assert(node);

	BTNode *x;
	if(node->left == NULL || node->right == NULL) {
		x = node;
	} else {
		x = bSTSuccessor(node);
	}
	BTNode *xChild;
	if(x->left != NULL) {
		xChild = x->left;
	} else {
		xChild = x->right;
	}
	BTNode *const xParent = x->parent;
	if(xParent == NULL) {
		*root = xChild;
	} else {
		if(x == xParent->left) {
			xParent->left = xChild;
		} else {
			xParent->right = xChild;
		}
	}
	if(xChild != NULL) {
		xChild->parent = xParent;
	}
	if(node != x) {
		swapNodeContent(node, x);
	}
	bTreeHeightFixUp(xParent);
	return x;
}

static void aVLTreeFixUp(BTNode **const root, BTNode *cur) {
	assert(root);
	assert(*root != NULL || cur == NULL);

	if(*root == NULL || cur == NULL) {
		return;
	}
	int const diff = bTreeHeight(cur->left)-bTreeHeight(cur->right);
	if(abs(diff) == 2) {
		BSTRotateDirection direction;
		if(diff == 2) { // left branch taller by 2
			direction = BST_ROTATE_RIGHT;
			BTNode **const y = &cur->left;
			if(bTreeHeight((*y)->left) < bTreeHeight((*y)->right)) {
				bSTRotate(y, BST_ROTATE_LEFT);
			}
		} else { // right branch taller by 2
			direction = BST_ROTATE_LEFT;
			BTNode **const y = &cur->right;
			if(bTreeHeight((*y)->right) < bTreeHeight((*y)->left)) {
				bSTRotate(y, BST_ROTATE_RIGHT);
			}
		}
		BTNode **x;
		if(cur == *root) {
			x = root;
		} else if(cur == cur->parent->left) {
			x = &cur->parent->left;
		} else {
			x = &cur->parent->right;
		}
		bSTRotate(x, direction);
		aVLTreeFixUp(root, cur->parent->parent);
	} else {
		aVLTreeFixUp(root, cur->parent);
	}
}

void aVLTreeAdd(BTNode **root, BTNode *const node) {
	assert(root);
	assert(node);
	assert(node->height = 1 && node->left == NULL && node->right == NULL);

	bSTreeAdd(root, node);
	aVLTreeFixUp(root, node);
}

BTNode *aVLTreeRemove(BTNode **root, BTNode *const node) {
	assert(root && *root);
	assert(node);

	BTNode *x = bSTreeRemove(root, node);
	aVLTreeFixUp(root, x->parent);
	return x;
}
