#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

////////////////////////////////////////////////////////////////////////
// declaration

struct Stack {
	int* array;
	int size;
	int pointer;
};

struct Stack* new_stack(int size);             //crea un nuovo stack vuoto di dimensione stabilita
bool push(struct Stack* stack, int element);   //prova ad inserire un elemento in stack, restituisce true se può, false altrimenti
int* pop(struct Stack* stack);                 //se lo stack è popolato rimuove un elemento e restituisce l'indirizzo ad esso, altrimenti NULL



////////////////////////////////////////////////////////////////////////
// test

int main(int argc, char** argv) {
	struct Stack* stack = new_stack(50);
	int i=3;
	int* element;
	
	push(stack, 0);
	push(stack, 1);
	element = pop(stack);
	printf("%d\n", *element);
	free(element);
	element = pop(stack);
	printf("%d\n", *element);
	free(element);
	element = pop(stack);
	printf("%p\n", element); //expected NULL
	push(stack, 2);
	
	while(push(stack, i)) {
		i++;
	}
	
	while((element = pop(stack)) != NULL) {
		printf("%d\n", *element);
		free(element);
	}
	
	free(stack);
	
	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

struct Stack* new_stack(int size) {
	if(size < 0) {
		return NULL;
	}
	struct Stack* stack = malloc(sizeof(struct Stack));
	
	stack->size = size;
	stack->pointer = 0;
	stack->array = malloc(sizeof(int) * size);
	
	return stack;
}

bool push(struct Stack* stack, int element) {
	if(stack->pointer >= stack->size) {      //se è pieno restituisci false
		return false;
	}
	
	stack->array[stack->pointer] = element;  //altrimenti inserisci l'elemento ed incrementa lo stack pointer
	stack->pointer++;
	
	return true;
}

int* pop(struct Stack* stack) {
	int* element = NULL;                 //inizializza element a NULL
	
	if(stack->pointer > 0) {             //se lo stack è popolato decresceil puntatore e
		stack->pointer--;                //salva in element un indirizzo all'elemento
		element = malloc(sizeof(int));
		*element = stack->array[stack->pointer];
	}
	
	return element;                      //se non era popolato element contiene ancora NULL, altrimenti contiene l'indirizzo valido
}
	
	
