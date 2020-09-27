#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

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
void remove_node(Node **tree, int key);
void swap_remove_successor(Node **tree, Node *cur);

int main(int argc, char** argv) {
    char command[7];
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
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    return 0;
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
        }
    } else if(key < (*tree)->key) {
        remove_node(&((*tree)->left), key);
    }else {
        remove_node(&((*tree)->right), key);
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
}

void show(Node *tree) {
    if(tree == NULL) {
        printf("NULL ");
    } else {
        printf("%d:%s ", tree->key, tree->val);
        show(tree->left);
        show(tree->right);
    }
}