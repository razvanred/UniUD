#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

//struttura del nodo dell'albero avl
struct AvlNode {
    long key;
    char *val;
    long height;
    struct AvlNode *left;
    struct AvlNode *right;
};

typedef struct AvlNode AvlNode;

void avl_show(AvlNode *tree);
void avl_insert(long key, char* val, AvlNode **tree);
void avl_clear(AvlNode *tree);
char *avl_find(AvlNode *tree, long key);
long height(AvlNode *tree);
void rebalance(AvlNode **tree);
long max(long a, long b);
void avl_rotate_left(AvlNode **tree);
void avl_rotate_right(AvlNode **tree);
void ready_left(AvlNode **tree);
void ready_right(AvlNode **tree);
void update_height(AvlNode *tree);

int main(int argc, char** argv) {
    char command[10];
    char val[50];
    long key;
    bool run = true;
    AvlNode *tree;
    tree = NULL;
    
    //parsing ed esecuzione del comando, ogni comando corrisponde ad una sola funzione (escluso l'eventuale tree=NULL)
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%ld %s", &key, (char*)&val);
            avl_insert(key, val, &tree);
        } else if(strcmp(command, "clear") == 0) {
            avl_clear(tree);
            tree = NULL;
        } else if(strcmp(command, "find") == 0) {
            scanf("%ld", &key);
            printf("%s\n", avl_find(tree, key));
        } else if(strcmp(command, "show") == 0) {
            avl_show(tree);
            puts("");
        } else if(strcmp(command, "height") == 0) {
            printf("%ld\n", height(tree));
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    return 0;
}

//calcola l'altezza del nodo da quella dei due figli
void update_height(AvlNode *tree) {
    if(tree != NULL){
        tree->height=max(height(tree->left),height(tree->right))+1;
    }
}

//se l'albero a sinistra è più alto che a destra, ruota a destra bilanciarlo
void ready_right(AvlNode **tree) {
    if(height((*tree)->left) > height((*tree)->right)) {
        avl_rotate_right(tree);
    }
}

//se l'albero a destra è più alto che a sinistra, ruota a sinistra bilanciarlo
void ready_left(AvlNode **tree) {
    if(height((*tree)->right) > height((*tree)->left)) {
        avl_rotate_left(tree);
    }
}

//ruota l'albero a destra ed aggiorna le altezze dei nodi modificati
void avl_rotate_right(AvlNode **tree) {
    AvlNode *temp = (*tree)->left;
    (*tree)->left = temp->right;
    temp->right = *tree;
    *tree = temp;
    update_height((*tree)->right);
    update_height((*tree)->left);
    update_height(*tree);
}

//ruota l'albero a sinistra ed aggiorna le altezze dei nodi modificati
void avl_rotate_left(AvlNode **tree) {
    AvlNode *temp = (*tree)->right;
    (*tree)->right = temp->left;
    temp->left = *tree;
    *tree = temp;
    update_height((*tree)->right);
    update_height((*tree)->left);
    update_height(*tree);
}

//massimo di due interi
long max(long a, long b) {
    return (a > b)?a:b;
}

//ribilancia un nodo dell'albero
void rebalance(AvlNode **tree) {
    long left = height((*tree)->left);
    long right = height((*tree)->right);
    
    //se destra o sinistra sono troppo alte ruoto nell'altra direzione per bilanciare
    if(left > right + 1) {
        ready_left(&((*tree)->left));
        avl_rotate_right(tree);
    } else if(right > left + 1) {
        ready_right(&((*tree)->right));
        avl_rotate_left(tree);
    } else {
    	//altrimenti aggiorno l'altezza
        update_height(*tree);
    }
}

//restituisce l'altezza dell'albero, se vuoto ha altezza 0
long height(AvlNode *tree) {
    return (tree == NULL)?0:tree->height;
}

//ricerca classica del nodo. Quando trova il nodo restituisce l'indirizzo al valore, NULL altrimenti
char *avl_find(AvlNode *tree, long key) {
	if (tree == NULL) {
		return NULL;
	} else if(key == tree->key) {
        return tree->val;
    } else if(key < tree->key) {
        return avl_find(tree->left, key);
    }else {
        return avl_find(tree->right, key);
    }
}

//scorre tutto l'albero e elimina valori e nodi in post-order
void avl_clear(AvlNode *tree) {
    if(tree != NULL) {
        avl_clear(tree->left);
        avl_clear(tree->right);
        free(tree->val);
        free(tree);
    }
}

void avl_insert(long key, char* val, AvlNode **tree) {
	//cerco uno spazio libero
    if(*tree == NULL){
    	//alloco il nodo
        *tree = malloc(sizeof(AvlNode));
        (*tree)->key = key;
        //alloco e copio il valore
        (*tree)->val = malloc(strlen(val));
        strcpy((*tree)->val, val);
        (*tree)->left = NULL;
        (*tree)->right = NULL;
    } else {
    	//altrimenti continuo a cercare
        if(key > (*tree)->key) {
            avl_insert(key, val, &((*tree)->right));
        } else {
            avl_insert(key, val, &((*tree)->left));
        }
    }
    //risalendo le chiamate mi assicuro che l'albero sia bilanciato e l'altezza corretta
    rebalance(tree);
}

//scorro l'albero e stampo in pre-order
void avl_show(AvlNode *tree) {
    if(tree == NULL) {
        printf("NULL ");
    } else {
        printf("%ld:%s:%ld ", tree->key, tree->val, tree->height);
        avl_show(tree->left);
        avl_show(tree->right);
    }
}
