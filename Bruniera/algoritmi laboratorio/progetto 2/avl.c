#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

struct Node {
    int key;
    char *val;
    int height;
    struct Node *left;
    struct Node *right;
};

typedef struct Node Node;

void show(Node *tree);
void insert(int key, char* val, Node **tree);
void clear(Node *tree);
char *find(Node *tree, int key);
void remove_node(Node **tree, int key);
void swap_remove_successor(Node **tree, Node *cur);
int height(Node *tree);
void rebalance(Node **tree);
int max(int a, int b);
void rotate_left(Node **tree);
void rotate_right(Node **tree);
void ready_left(Node **tree);
void ready_right(Node **tree);
void update_height(Node *tree);

int main(int argc, char** argv) {
    char command[10];
    char val[50];
    int key;
    bool run = true;
    Node *tree;
    tree = NULL;
    
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%d %s", &key, (char*)&val);
            insert(key, val, &tree);
        } else if(strcmp(command, "clear") == 0) {
            clear(tree);
            tree = NULL;
        } else if(strcmp(command, "remove") == 0) {
            scanf("%d", &key);
            remove_node(&tree, key);
        } else if(strcmp(command, "find") == 0) {
            scanf("%d", &key);
            printf("%s\n", find(tree, key));
        } else if(strcmp(command, "show") == 0) {
            show(tree);
            puts("");
        } else if(strcmp(command, "height") == 0) {
            printf("%d\n", height(tree));
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    return 0;
}

void update_height(Node *tree) {
    if(tree != NULL){
        tree->height=max(height(tree->left),height(tree->right))+1;
    }
}

void ready_right(Node **tree) {
    if(height((*tree)->left) > height((*tree)->right)) {
        rotate_right(tree);
    }
}

void ready_left(Node **tree) {
    if(height((*tree)->right) > height((*tree)->left)) {
        rotate_left(tree);
    }
}

void rotate_right(Node **tree) {
    Node *temp = (*tree)->left;
    (*tree)->left = temp->right;
    temp->right = *tree;
    *tree = temp;
    update_height((*tree)->right);
    update_height((*tree)->left);
    update_height(*tree);
}

void rotate_left(Node **tree) {
    Node *temp = (*tree)->right;
    (*tree)->right = temp->left;
    temp->left = *tree;
    *tree = temp;
    update_height((*tree)->right);
    update_height((*tree)->left);
    update_height(*tree);
}

int max(int a, int b) {
    return (a > b)?a:b;
}

void rebalance(Node **tree) {
    int left = height((*tree)->left);
    int right = height((*tree)->right);
    
    if(left > right + 1) {
        ready_left(&((*tree)->left));
        rotate_right(tree);
    } else if(right > left + 1) {
        ready_right(&((*tree)->right));
        rotate_left(tree);
    } else {
        update_height(*tree);
    }
}

int height(Node *tree) {
    return (tree == NULL)?0:tree->height;
}

void swap_remove_successor(Node **tree, Node *cur) {
    if((*tree)->left == NULL) {
        cur->key = (*tree)->key;
        free(cur->val);
        cur->val = (*tree)->val;
        Node *temp;
        temp = *tree;
        *tree = temp->right;
        free(temp);
    } else {
        swap_remove_successor(&((*tree)->left), cur);
        rebalance(tree);
    }
}

void remove_node(Node **tree, int key) {
    if(key == (*tree)->key) {
        if((*tree)->right == NULL) {
            Node *cur;
            cur = *tree;
            *tree = cur->left;
            free(cur->val);
            free(cur);
        } else if((*tree)->left == NULL) {
            Node *cur;
            cur = *tree;
            *tree = cur->right;
            free(cur->val);
            free(cur);
        } else {
            swap_remove_successor(&((*tree)->right), *tree);
            rebalance(tree);
        }
    } else if(key < (*tree)->key) {
        remove_node(&((*tree)->left), key);
        rebalance(tree);
    }else {
        remove_node(&((*tree)->right), key);
        rebalance(tree);
    }
}

char *find(Node *tree, int key) {
    if(key == tree->key) {
        return tree->val;
    } else if(key < tree->key) {
        return find(tree->left, key);
    }else {
        return find(tree->right, key);
    }
}

void clear(Node *tree) {
    if(tree != NULL) {
        clear(tree->left);
        clear(tree->right);
        free(tree->val);
        free(tree);
    }
}

void insert(int key, char* val, Node **tree) {
    if(*tree == NULL){
        *tree = malloc(sizeof(Node));
        (*tree)->key = key;
        (*tree)->val = malloc(strlen(val));
        strcpy((*tree)->val, val);
        (*tree)->left = NULL;
        (*tree)->right = NULL;
    } else {
        if(key > (*tree)->key) {
            insert(key, val, &((*tree)->right));
        } else {
            insert(key, val, &((*tree)->left));
        }
    }
    rebalance(tree);
}

void show(Node *tree) {
    if(tree == NULL) {
        printf("NULL ");
    } else {
        printf("%d:%s:%d ", tree->key, tree->val, tree->height);
        show(tree->left);
        show(tree->right);
    }
}