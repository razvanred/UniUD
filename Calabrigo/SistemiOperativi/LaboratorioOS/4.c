/*
Riscrivere le funzioni length(), find(), last(), append(), e
destroy() per le liste concatenate utilizzando un approccio
ricorsivo.
*/
#include<stdio.h>
#include<stdbool.h>
#include<stdlib.h>

struct node{
    int data;
    struct node *next;
};

int length(struct node *root);
bool find(struct node *root, int data);
void* last(struct node *root);
bool add(struct node *root, int data);
bool destroy(struct node *root);

int main(){
    struct node *root = malloc(sizeof(struct node));
    root->data = 0;
    root->next = NULL;
    add(root, 5);
    add(root, 7);
    add(root, 3);
    printf("len: %d\n", length(root));
    int num = 3;
    printf("find %d = %d\n", num, find(root, num));
    destroy(root);
    return 0;
}

bool add(struct node *root, int data){
    struct node *c = last(root);
    struct node *newNode = malloc(sizeof(struct node));
    newNode->data = data;
    newNode->next = NULL;
    c->next = newNode;
    return true;
}

void* last(struct node *root){
    struct node *c = root;
    while(c->next != NULL){
        c = c->next;
    }
    return c;
}

int length(struct node *root){
    int counter = 0;

    struct node *c = root;
    while(c->next != NULL){
        c = c->next;
        counter++;
    }
    return counter;
}

bool find(struct node *root, int data){
    struct node *c = root;
    while(c->next != NULL){
        if(c->data == data){
            return true;
        }
        c = c->next;
    }
    if(c->data == data){
        return true;
    }
    return false;
}

bool destroy(struct node *root){

    struct node *c = root;
    struct node *temp = NULL;
    while(c->next != NULL){
        temp = c;
        c = c->next;
        free(temp);
    }
    return true;
}

