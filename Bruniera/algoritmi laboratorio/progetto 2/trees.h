#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define RED 1
#define BLACK 0
typedef unsigned char color;

//struttura del nodo dell'albero
struct Node {
    int key;
    char *val;
    struct Node *left;
    struct Node *right;
};

typedef struct Node Node;

//struttura del nodo dell'albero avl
struct AvlNode {
    int key;
    char *val;
    int height;
    struct AvlNode *left;
    struct AvlNode *right;
};

typedef struct AvlNode AvlNode;

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

//BST
void show(Node *tree);
void insert(int key, char* val, Node **tree);
void clear(Node *tree);
char *find(Node *tree, int key);

//AVL
void avl_show(AvlNode *tree);
void avl_insert(int key, char* val, AvlNode **tree);
void avl_clear(AvlNode *tree);
char *avl_find(AvlNode *tree, int key);
int height(AvlNode *tree);
void rebalance(AvlNode **tree);
int max(int a, int b);
void avl_rotate_left(AvlNode **tree);
void avl_rotate_right(AvlNode **tree);
void ready_left(AvlNode **tree);
void ready_right(AvlNode **tree);
void update_height(AvlNode *tree);

//RBT
void rbt_show(RbtNode *tree);
void rbt_insert(int key, char* val, RbtNode **tree);
void rbt_insert_rec(int key, char* val, RbtNode *father, RbtNode **tree, RbtNode **root);
void rbt_clear(RbtNode *tree);
char *rbt_find(RbtNode *tree, int key);
void rbt_rotate_left(RbtNode *tree, RbtNode **root);
void rbt_rotate_right(RbtNode *tree, RbtNode **root);
color rbt_color(RbtNode *tree);
void rbt_fix(RbtNode *tree, RbtNode **root);

///////////////
//BST

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

///////////////
//AVL

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
int max(int a, int b) {
    return (a > b)?a:b;
}

//ribilancia un nodo dell'albero
void rebalance(AvlNode **tree) {
    int left = height((*tree)->left);
    int right = height((*tree)->right);
    
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
int height(AvlNode *tree) {
    return (tree == NULL)?0:tree->height;
}

//ricerca classica del nodo. Quando trova il nodo restituisce l'indirizzo al valore, NULL altrimenti
char *avl_find(AvlNode *tree, int key) {
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

void avl_insert(int key, char* val, AvlNode **tree) {
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
        printf("%d:%s:%d ", tree->key, tree->val, tree->height);
        avl_show(tree->left);
        avl_show(tree->right);
    }
}

///////////////
//RBT

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
				if(rbt_color(father->left) == RED) {
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
    if(temp != NULL) {
    	temp->father = tree;
	}
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
    if(temp != NULL) {
    	temp->father = tree;
	}
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
