#include "vector.h"

//la Heap è implementata in due varianti, MaxHeap e MinHeap, per lo più analoghe ma,
//come suggerisce il nome, una avrà alla radice il massimo e l'altra il minimo.
//La maxheap sfrutta la classe astratta Vector e non è a conoscenza dell'implementazione
//perciò può usare solo i metodi incapsulati nella struttura Vector, che saranno sufficinti
//ad implementare correttamente le due versioni di Heap
struct Heap {
	T (*get)(struct Heap*, int);
	T (*pop)(struct Heap*);
	void (*push)(struct Heap*, T);
	void (*delete)(struct Heap*);
	int heapsize;
	Vector *vec;
};

typedef struct Heap Heap;

//alcune funzioni e metodi indipendenti dall'implementazione
int heap_parent(int root) {
	return (root - 1) >> 1;
}

int heap_right(int root) {
	return (root + 1) << 1;
}

int heap_left(int root) {
	return (root << 1) + 1;
}

//il vettore è sempre esterno alla struttura, non viene liberato
void heap_delete(Heap *self) {}

T heap_get(Heap *self, int index) {
	Vector *v = self->vec;
	return v->get(v, index);
}

///// MaxHeap
//heapify per la MaxHeap analogo alla MinHeap ma tenendo il massimo come radice della heap
void maxheap_heapify(Heap *heap, int root) {
	Vector *v = heap->vec;
	int max = root;
	
	//trovo il massimo tra la radice ed i due figli 
	if(heap_left(root) < heap->heapsize && v->compare(v, max, heap_left(root)) < 0) { 
		max = heap_left(root);
	}
	if(heap_right(root) < heap->heapsize && v->compare(v, max, heap_right(root)) < 0) {
		max = heap_right(root);
	}
	
	//se non è la radice scambio figlio massimo e radice e ricorro sul figlio massimo 
	if(max != root) {
		v->swap(v, root, max);
		maxheap_heapify(heap, max);
	}
	//altrimenti termina
}

//il metodo è analogo a quello della MinHeap, ma chiama le varianti per MaxHeap delle funzioni
T maxheap_pop(Heap *self) {
	T element = self->get(self, 0);
	self->heapsize--;
	self->vec->swap(self->vec, 0, self->heapsize);
	maxheap_heapify(self, 0);
	
	return element;
}

//il metodo è analogo a quello della MinHeap, ma chiama le varianti per MaxHeap delle funzioni,
//e porta in cima alla heap il massimo
void maxheap_push(Heap *self, T element) {
	Vector *v = self->vec;
	int index = self->heapsize;
	
	self->heapsize++;
	v->set(v, index, element);
	
	while(index > 0 && v->compare(v, index, heap_parent(index)) > 0) {
		v->swap(v, index, heap_parent(index));
		index = heap_parent(index);
	}
}

//costruisce una heap da un vettore già popolato, quindi lanciando heapify per ordinarne i valori
Heap maxheap_from(Vector *v) {
	//valorizza una struttura MaxHeap
	Heap h = (Heap) {
		.get = heap_get,
		.pop = maxheap_pop,
		.push = maxheap_push,
		.delete = heap_delete,
		.heapsize = v->size,
		.vec = v,
	};
	
	//riordina i valori del vettore
	for(int i = v->size / 2; i >= 0; i--) {
		maxheap_heapify(&h, i);
	}
	
	//restituisce la struttura
	return h;
}

//costruisce una heap da un vettore vuoto, quindi imposta heapsize a 0
Heap maxheap_with(Vector *v) {
	return (Heap) {
		.get = heap_get,
		.pop = maxheap_pop,
		.push = maxheap_push,
		.delete = heap_delete,
		.heapsize = 0,
		.vec = v,
	};
}

///// MinHeap
//heapify per la MinHeap analogo alla MaxHeap ma tenendo il minimo come radice della heap
void minheap_heapify(Heap *heap, int root) {
	Vector *v = heap->vec;
	int min = root;
	
	//trovo il minimo tra la radice ed i due figli 
	if(heap_left(root) < heap->heapsize && v->compare(v, min, heap_left(root)) > 0) { 
		min = heap_left(root);
	}
	if(heap_right(root) < heap->heapsize && v->compare(v, min, heap_right(root)) > 0) {
		min = heap_right(root);
	}
	
	//se non è la radice scambio figlio minimo e radice e ricorro sul figlio minimo 
	if(min != root) {
		v->swap(v, root, min);
		minheap_heapify(heap, min);
	}
	//altrimenti termina
}

//il metodo è analogo a quello della MaxHeap, ma chiama le varianti per MinHeap delle funzioni
T minheap_pop(Heap *self) {
	T element = self->get(self, 0);
	self->heapsize--;
	self->vec->swap(self->vec, 0, self->heapsize);
	minheap_heapify(self, 0);
	
	return element;
}

//il metodo è analogo a quello della MaxHeap, ma chiama le varianti per MinHeap delle funzioni,
//e porta in cima alla heap il minimo
void minheap_push(Heap *self, T element) {
	Vector *v = self->vec;
	int index = self->heapsize;
	
	self->heapsize++;
	v->set(v, index, element);
	
	while(index > 0 && v->compare(v, index, heap_parent(index)) < 0) {
		v->swap(v, index, heap_parent(index));
		index = heap_parent(index);
	}
}

//costruisce una heap da un vettore già popolato, quindi lanciando heapify per ordinarne i valori
Heap minheap_from(Vector *v) {
	//valorizza una struttura MinHeap
	Heap h = (Heap) {
		.get = heap_get,
		.pop = minheap_pop,
		.push = minheap_push,
		.delete = heap_delete,
		.heapsize = v->size,
		.vec = v,
	};
	
	//riordina i valori del vettore
	for(int i = v->size / 2; i >= 0; i--) {
		minheap_heapify(&h, i);
	}
	
	//restituisce la struttura
	return h;
}

//costruisce una heap da un vettore vuoto, quindi imposta heapsize a 0
Heap minheap_with(Vector *v) {
	return (Heap) {
		.get = heap_get,
		.pop = minheap_pop,
		.push = minheap_push,
		.delete = heap_delete,
		.heapsize = 0,
		.vec = v,
	};
}

