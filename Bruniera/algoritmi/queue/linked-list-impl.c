#include <stdio.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// declaration

struct Node {
	int key;
	struct Node* next;
};

struct Queue {
	struct Node* head;
	struct Node* tail;
};
	
struct Queue* new_queue();                          //crea una nuova coda vota
struct Node* new_node(int key);                     //crea un nuovo nodo col valore passato come chiave
void push(struct Queue* queue, struct Node* node);  //inserisce un nodo in coda
struct Node* pop(struct Queue* queue);              //rimuove un nodo dalla testa della lista e lo restituisce



////////////////////////////////////////////////////////////////////////
// test

int main(int argc, char** argv){
	struct Queue* queue = new_queue();
	struct Node* node = NULL;
	
	push(queue, new_node(0));
	push(queue, new_node(1));
	node = pop(queue);
	printf("%d\n", node->key);
	free(node);
	node = pop(queue);
	printf("%d\n", node->key);
	free(node);
	node = pop(queue);
	printf("%p\n", node); //expected NULL
	push(queue, new_node(2));
	
	for(int i = 3; i < 50; i++) {
		push(queue, new_node(i));
	}
	
	while((node = pop(queue)) != NULL) {
		printf("%d\n", node->key);
		free(node);
	}
	
	free(queue);
	
	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

struct Queue* new_queue() {
	struct Queue* queue = malloc(sizeof(struct Queue));
	
	queue->head = NULL;
	queue->tail = NULL;
	
	return queue;
}

struct Node* new_node(int key) {
	struct Node* node = malloc(sizeof(struct Node));
	
	node->key = key;
	node->next = NULL;
	
	return node;
}

void push(struct Queue* queue, struct Node* node) {
	if(queue->tail == NULL) {           //se è vuota imposta anche head
		queue -> tail = node;
		queue -> head = node;
	} else {                            //altrimenti agisci solo su tail
		queue->tail->next = node;
		queue->tail = node;
	}
}

struct Node* pop(struct Queue* queue) {
	struct Node* head = queue->head;
	
	if(head == NULL) {                 //se è vuota restituisci NULL
		return NULL;
	} else if(head == queue->tail) {   //se ha un solo elemento imposta anche tail
		queue->head = NULL;
		queue->tail = NULL;
	} else {                           //altrimenti agisci solo su head
		queue->head = head->next;
	}
	
	return head;
}
