#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define RED 1
#define BLACK 0
typedef unsigned char color;

//struttura del nodo dell'albero
struct RbtNode {
    int key;
    char *val;
    color col;
    struct RbtNode *left;
    struct RbtNode *right;
};

typedef struct RbtNode RbtNode;

void rbt_show(RbtNode *tree);
void rbt_insert(int key, char* val, RbtNode **tree);
void rbt_clear(RbtNode *tree);
char *rbt_find(RbtNode *tree, int key);
void rbt_rotate_left(RbtNode **tree);
void rbt_rotate_right(RbtNode **tree);
color rbt_color(RbtNode *tree);
void rbt_fix(RbtNode **tree);

int main(int argc, char** argv) {
    char command[7];
    char val[256];
    int key;
    bool run = true;
    RbtNode *tree;
    tree = NULL;
    
    //parsing ed esecuzione del comando, ogni comando corrisponde ad una sola funzione (escluso l'eventuale tree=NULL)
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%d %s", &key, &val[0]);
            rbt_insert(key, val, &tree);
        } else if(strcmp(command, "clear") == 0) {
            rbt_clear(tree);
            tree = NULL;
        } else if(strcmp(command, "find") == 0) {
            scanf("%d", &key);
            printf("%s\n", rbt_find(tree, key));
        } else if(strcmp(command, "show") == 0) {
            rbt_show(tree);
            puts("");
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    return 0;
}

void rbt_fix(RbtNode **tree) {
	if((*tree)->col == RED) {
		if((*tree)->left->col == RED) {
			
		}
	}
}

//resitisce il colore dell'albero, BLACK se NULL
color rbt_color(RbtNode *tree) {
	return (tree == NULL)?BLACK:tree->col;
}

//ruota l'albero a destra
void rbt_rotate_right(RbtNode **tree) {
    RbtNode *temp = (*tree)->left;
    (*tree)->left = temp->right;
    temp->right = *tree;
    *tree = temp;
}

//ruota l'albero a sinistra
void rbt_rotate_left(RbtNode **tree) {
    RbtNode *temp = (*tree)->right;
    (*tree)->right = temp->left;
    temp->left = *tree;
    *tree = temp;
}

//ricerca classica del nodo. Quando trova il nodo restituisce l'indirizzo al valore, NULL altrimenti
char *rbt_find(RbtNode *tree, int key) {
    if(tree == NULL) {
    	return NULL;
	} else if(key == tree->key) {
        return tree->val;
    } else if(key < tree->key) {
        return rbt_find(tree->left, key);
    }else {
        return rbt_find(tree->right, key);
    }
}

//scorre tutto l'albero e elimina valori e nodi in post-order
void rbt_clear(RbtNode *tree) {
    if(tree != NULL) {
        rbt_clear(tree->left);
        rbt_clear(tree->right);
        free(tree->val);
        free(tree);
    }
}

void rbt_insert(int key, char* val, RbtNode **tree) {
	//cerco uno spazio libero
    if(*tree == NULL){
    	//alloco il nodo
        *tree = malloc(sizeof(RbtNode));
        (*tree)->key = key;
        (*tree)->col = RED;
        //alloco e copio il valore
        (*tree)->val = malloc(strlen(val));
        strcpy((*tree)->val, val);
        (*tree)->left = NULL;
        (*tree)->right = NULL;
    } else {
    	//altrimenti prosegue la ricerca
        if(key > (*tree)->key) {
            rbt_insert(key, val, &((*tree)->right));
        } else {
            rbt_insert(key, val, &((*tree)->left));
        }
        rbt_fix(tree);
    }
}

//scorro l'albero e stampo in pre-order
void rbt_show(RbtNode *tree) {
    if(tree == NULL) {
        printf("NULL ");
    } else {
        printf("%d:%s ", tree->key, tree->val);
        rbt_show(tree->left);
        rbt_show(tree->right);
    }
}
