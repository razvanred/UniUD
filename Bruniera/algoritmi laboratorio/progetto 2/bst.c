#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

//struttura del nodo dell'albero
struct Node {
    int key;
    char *val;
    struct Node *left;
    struct Node *right;
};

typedef struct Node Node;

void show(Node *tree);
void insert(int key, char* val, Node **tree);
void clear(Node *tree);
char *find(Node *tree, int key);

int main(int argc, char** argv) {
    char command[7];
    char val[256];
    int key;
    bool run = true;
    Node *tree;
    tree = NULL;
    
    //parsing ed esecuzione del comando, ogni comando corrisponde ad una sola funzione (escluso l'eventuale tree=NULL)
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%d %s", &key, &val[0]);
            insert(key, val, &tree);
        } else if(strcmp(command, "clear") == 0) {
            clear(tree);
            tree = NULL;
        } else if(strcmp(command, "find") == 0) {
            scanf("%d", &key);
            printf("%s\n", find(tree, key));
        } else if(strcmp(command, "show") == 0) {
            show(tree);
            puts("");
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    return 0;
}

//ricerca classica del nodo. Quando trova il nodo restituisce l'indirizzo al valore, NULL altrimenti
char *find(Node *tree, int key) {
    if(tree == NULL) {
    	return NULL;
	} else if(key == tree->key) {
        return tree->val;
    } else if(key < tree->key) {
        return find(tree->left, key);
    }else {
        return find(tree->right, key);
    }
}

//scorre tutto l'albero e elimina valori e nodi in post-order
void clear(Node *tree) {
    if(tree != NULL) {
        clear(tree->left);
        clear(tree->right);
        free(tree->val);
        free(tree);
    }
}

void insert(int key, char* val, Node **tree) {
	//cerco uno spazio libero
    if(*tree == NULL){
    	//alloco il nodo
        *tree = malloc(sizeof(Node));
        (*tree)->key = key;
        //alloco e copio il valore
        (*tree)->val = malloc(strlen(val));
        strcpy((*tree)->val, val);
        (*tree)->left = NULL;
        (*tree)->right = NULL;
    } else {
    	//altrimenti prosegue la ricerca
        if(key > (*tree)->key) {
            insert(key, val, &((*tree)->right));
        } else {
            insert(key, val, &((*tree)->left));
        }
    }
}

//scorro l'albero e stampo in pre-order
void show(Node *tree) {
    if(tree == NULL) {
        printf("NULL ");
    } else {
        printf("%d:%s ", tree->key, tree->val);
        show(tree->left);
        show(tree->right);
    }
}
