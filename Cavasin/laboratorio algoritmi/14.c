#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "trees.h"

#define INPUT_SIZE 10000

extern void readLine(char s[], int sLength);

extern BTNode *bTNodeAlloc(int key, char const val[const]);
extern void bTreeFree(BTNode **root);
extern void bTreePrePrint(BTNode const *cur, void (*print)(BTNode const *));
extern BTNode *bSTFind(BTNode *cur, int key);
extern void aVLTreeAdd(BTNode **root, BTNode *node);
extern BTNode *aVLTreeRemove(BTNode **root, BTNode *node);

typedef enum CommandType {
	INSERT, REMOVE, FIND, CLEAR, SHOW, EXIT
} CommandType;

typedef struct Command {
	CommandType const type;
	int const key;
	char value[TREE_VALUE_SIZE];
} Command;

void printNode(BTNode const *const node) {
	if(node == NULL) {
		printf("NULL ");
		return;
	}
	printf("%d:%s:%d ", node->key, node->val, node->height);
}

void printNodeValue(BTNode const *const node) {
	if(node == NULL) {
		printf("NULL ");
		return;
	}
	printf("%s ", node->val);
}

Command parse(char s[const]) {
	CommandType commandType;
	int key = 0;
	char value[TREE_VALUE_SIZE] = "";

	char const *const commandTypeString = strtok(s, " ");
	if(commandTypeString == NULL) { // NOLINT(bugprone-branch-clone)
		commandType = EXIT;
	} else if(strcmp("insert", commandTypeString) == 0) {
		commandType = INSERT;
	} else if(strcmp("remove", commandTypeString) == 0) {
		commandType = REMOVE;
	} else if(strcmp("find", commandTypeString) == 0) {
		commandType = FIND;
	} else if(strcmp("clear", commandTypeString) == 0) {
		commandType = CLEAR;
	} else if(strcmp("show", commandTypeString) == 0) {
		commandType = SHOW;
	} else {
		commandType = EXIT;
	}

	if(commandType == INSERT || commandType == REMOVE || commandType == FIND) {
		char const *const keyString = strtok(NULL, " ");
		if(keyString == NULL) {
			commandType = EXIT;
		} else {
			key = atoi(keyString); // NOLINT(cert-err34-c)
			if(commandType == INSERT) {
				char const *const valueString = strtok(NULL, " ");
				if(valueString == NULL) {
					commandType = EXIT;
				} else {
					strncpy(value, valueString, TREE_VALUE_SIZE);
				}
			}
		}
	}
	Command command = (Command){.type=commandType, .key=key};
	strcpy(command.value, value);
	return command;
}

int main(void) {
	char s[INPUT_SIZE];
	BTNode *avlTree = NULL;

	while(true) {
		readLine(s, INPUT_SIZE);
		Command const command = parse(s);

		switch(command.type) {
			case INSERT:
				aVLTreeAdd(&avlTree, bTNodeAlloc(command.key, command.value));
				break;
			case REMOVE: {
				BTNode *const node = aVLTreeRemove(&avlTree, bSTFind(avlTree, command.key));
				free(node);
			}
				break;
			case FIND: {
				printNodeValue(bSTFind(avlTree, command.key));
			}
				break;
			case CLEAR:
				bTreeFree(&avlTree);
				break;
			case SHOW:
				bTreePrePrint(avlTree, printNode);
				break;
			default:
				bTreeFree(&avlTree);
				return 0;
		}
	}
}
