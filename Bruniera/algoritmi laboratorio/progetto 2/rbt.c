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
    struct RbtNode *father;
    struct RbtNode *left;
    struct RbtNode *right;
};

typedef struct RbtNode RbtNode;

void rbt_show(RbtNode *tree);
void rbt_insert(int key, char* val, RbtNode **tree);
void rbt_insert_rec(int key, char* val, RbtNode *father, RbtNode **tree, RbtNode **root);
void rbt_clear(RbtNode *tree);
char *rbt_find(RbtNode *tree, int key);
void rbt_rotate_left(RbtNode *tree, RbtNode **root);
void rbt_rotate_right(RbtNode *tree, RbtNode **root);
color rbt_color(RbtNode *tree);
void rbt_fix(RbtNode *tree, RbtNode **root);

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
            scanf("%d %s", &key, (char*) val);
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

void rbt_fix(RbtNode *tree, RbtNode **root) {
	if(tree != NULL){
		if (tree->father == NULL) {
			//se è la radice coloro di nero e esco
			tree->col = BLACK;
		} else if(rbt_color(tree) == RED && rbt_color(tree->father) == RED) {
			//se il nodo è rosso con padre rosso
			RbtNode *father =  tree->father;
			RbtNode *grandfather =  tree->father->father;
			if(rbt_color(grandfather->left) == RED) {
				//se il figlio sinistro del nonno è rosso
				if(rbt_color(grandfather->right) == RED) {
					//e anche il destro
					//caso sfortunato
					grandfather->col = RED;
					grandfather->left->col = BLACK;
					grandfather->right->col = BLACK;
					rbt_fix(grandfather, root);
				} else {
					//altrimenti, dato che il padre è rosso, il destro è lo zio
					if(rbt_color(father->right) == RED) {
						//se il figlio rosso del padre (quindi il nodo) è a destra
						//caso quasi fortunato
						rbt_rotate_left(father, root);
					}
					//adesso è il caso fortunato
					rbt_rotate_right(grandfather, root);
					grandfather->col = RED;
					grandfather->father->col = BLACK;
				}
			} else {
				//altrimenti, dato che il padre è rosso, il sinistro è lo zio
				if(rbt_color(father->right) == RED) {
					//se il figlio rosso del padre (quindi il nodo) è a destra
					//caso quasi fortunato
					rbt_rotate_right(father, root);
				}
				//adesso è il caso fortunato
				rbt_rotate_left(grandfather, root);
				grandfather->col = RED;
				grandfather->father->col = BLACK;
			}
		}
	}
	//se è nero, radice, o non esiste esco
}

//resitisce il colore dell'albero, BLACK se NULL
color rbt_color(RbtNode *tree) {
	return (tree == NULL)?BLACK:tree->col;
}

//ruota l'albero a destra
void rbt_rotate_right(RbtNode *tree, RbtNode **root) {
    RbtNode *temp = tree->left->right;
    tree->left->right = tree;
    tree->left->father = tree->father;
    tree->father = tree->left;
    tree->left = temp;
    if(tree->father->father != NULL) {
    	if(tree->father->father->right == tree) {
    		tree->father->father->right = tree->father;
		} else {
    		tree->father->father->left = tree->father;
		}
	} else {
		*root = tree->father;
	}
}

//ruota l'albero a sinistra
void rbt_rotate_left(RbtNode *tree, RbtNode **root) {
    RbtNode *temp = tree->right->left;
    tree->right->left = tree;
    tree->right->father = tree->father;
    tree->father = tree->right;
    tree->right = temp;
    if(tree->father->father != NULL) {
    	if(tree->father->father->right == tree) {
    		tree->father->father->right = tree->father;
		} else {
    		tree->father->father->left = tree->father;
		}
	} else {
		*root = tree->father;
	}
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

//funzione d'appoggio, dichiarata inline per suggerire al compilatore di espanderla come una macro
void inline rbt_insert(int key, char* val, RbtNode **tree) {
	//richiama la funzione ricorsiva passando il valore iniziale NULL come indirizzo del padre
	rbt_insert_rec(key, val, NULL, tree, tree);
}

void rbt_insert_rec(int key, char* val, RbtNode *father, RbtNode **tree, RbtNode **root) {
	//cerco uno spazio libero
    if(*tree == NULL){
    	//alloco il nodo
        *tree = malloc(sizeof(RbtNode));
        (*tree)->key = key;
        if(father == NULL){
        	(*tree)->col = BLACK;	
		} else {
        	(*tree)->col = RED;
		}
        //alloco e copio il valore
        (*tree)->val = malloc(strlen(val));
        strcpy((*tree)->val, val);
        (*tree)->left = NULL;
        (*tree)->right = NULL;
        (*tree)->father = father;
        rbt_fix(*tree, root);
    } else {
    	//altrimenti prosegue la ricerca
        if(key > (*tree)->key) {
            rbt_insert_rec(key, val, (*tree), &((*tree)->right), root);
        } else {
            rbt_insert_rec(key, val, (*tree), &((*tree)->left), root);
        }
    }
}

//scorro l'albero e stampo in pre-order
void rbt_show(RbtNode *tree) {
    if(tree == NULL) {
        printf("NULL ");
    } else {
        printf("%d:%s:%s ", tree->key, tree->val, (tree->col == BLACK)?"black":"red");
        rbt_show(tree->left);
        rbt_show(tree->right);
    }
}
