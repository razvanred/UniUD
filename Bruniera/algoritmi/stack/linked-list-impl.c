#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

////////////////////////////////////////////////////////////////////////
// declaration

struct Node {
	int key;
	struct Node* next;
};

typedef struct Node* Stack;

Stack* new_stack();                          //crea un nuovo stack vuoto
struct Node* new_node(int key);              //crea un nuovo nodo col valore passato come chiave
void push(Stack* stack, struct Node* node);  //inserisce un nodo in stack
struct Node* pop(Stack* stack);              //rimuove un nodo dallo stack e lo restituisce



////////////////////////////////////////////////////////////////////////
// test

int main(int argc, char** argv) {
	Stack* stack = new_stack();
	struct Node* node = NULL;
	
	push(stack, new_node(0));
	push(stack, new_node(1));
	node = pop(stack);
	printf("%d\n", node->key);
	free(node);
	node = pop(stack);
	printf("%d\n", node->key);
	free(node);
	node = pop(stack);
	printf("%p\n", node); //expected NULL
	push(stack, new_node(2));
	
	for(int i = 3; i < 50; i++) {
		push(stack, new_node(i));
	}
	
	while((node = pop(stack)) != NULL) {
		printf("%d\n", node->key);
		free(node);
	}
	
	free(stack);
	
	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

Stack* new_stack() {
	Stack* stack = malloc(sizeof(Stack));
	*stack = NULL;
	return stack;
}

struct Node* new_node(int key) {
	struct Node* node = malloc(sizeof(struct Node));
	
	node->key = key;
	node->next = NULL;
	
	return node;
}

void push(Stack* stack, struct Node* node) {
	node->next = *stack;                //incoda lo stack al nuovo nodo
	*stack = node;                      //ora la cima dello stack punta al nuovo nodo
}

struct Node* pop(Stack* stack) {
	struct Node* node = NULL;           //setta node a NULL
	
	if(*stack != NULL){                 //se lo stack è popolato salva il primo nodo su node e
		node = *stack;                  //sposta lo stack pointer al nodo successivo
		*stack = (*stack)->next;
	}
	
	return node;                        //se non c'erano elementi node vale ancora NULL
}
