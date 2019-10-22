#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

////////////////////////////////////////////////////////////////////////
// declaration

struct Queue {
	int first;
	int next;
	int* array;
	int size;
	bool empty;
};

struct Queue* new_queue(int size);             //crea una nuova coda vuota di dimensione stabilita. se size<1 restituisce NULL
bool push(struct Queue* queue, int element);   //prova ad inserire un elemento in coda, restituisce true se può, false altrimenti
int* pop(struct Queue* queue);                 //se la coda è popolata rimuove un elemento e restituisce l'indirizzo ad esso, altrimenti NULL



////////////////////////////////////////////////////////////////////////
// test

int main(int argc, char** argv) {
	struct Queue* queue = new_queue(50);
	int i=3;
	int* element;
	
	push(queue, 0);
	push(queue, 1);
	element = pop(queue);
	printf("%d\n", *element);
	free(element);
	element = pop(queue);
	printf("%d\n", *element);
	free(element);
	element = pop(queue);
	printf("%p\n", element); //expected NULL
	push(queue, 2);
	
	while(push(queue, i)) {
		i++;
	}
	
	while((element = pop(queue)) != NULL) {
		printf("%d\n", *element);
		free(element);
	}
	
	free(queue);
	
	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

struct Queue* new_queue(int size) {
	if(size < 1) {          //controlla la dimensione. se >=1 crea la coda
		return NULL;
	}
	struct Queue* queue = malloc(sizeof(struct Queue));
	
	queue->size = size;
	queue->empty = true;
	queue->first = 0;
	queue->next = 0;
	queue->array = malloc(sizeof(int) * size);
	
	return queue;
}

bool push(struct Queue* queue, int element) {
	if(!(queue->empty) && (queue->next == queue->first)) { //se è pieno ritorna falso
		return false;
	} 
	queue->empty = false;                                  //altrimenti segna che non è vuoto,
	queue->array[queue->next] = element;                   //inserisci e sposta l'indice di uno
	queue->next = (queue->next + 1) % queue->size;
	return true;
}

int* pop(struct Queue* queue) {
	int* element = NULL;                                   //inizializza a null
	
	if(!(queue->empty)) {                                  //se non è vuota
		element = malloc(sizeof(int));                     //salva l'elemento in heap e sposta l'indice di 1
		*element = queue->array[queue->first];
		queue->first = (queue->first + 1) % queue->size;

		if(queue->first == queue->next) {                  //se gli indici sono uguali è stata svuotata
			queue->empty = true;
		}
	}
	
	return element;                                        //restituisce element, se era vuoto vale ancora NULL
}
