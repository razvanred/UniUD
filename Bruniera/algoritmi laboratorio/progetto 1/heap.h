#include "vector.h"

struct Heap {
	T (*get)(struct Heap*, int);
	T (*pop)(struct Heap*);
	void (*push)(struct Heap*, T);
	void (*delete)(struct Heap*);
	int heapsize;
	Vector *vec;
};

typedef struct Heap Heap;

int heap_parent(int root) {
	return (root - 1) >> 1;
}

int heap_right(int root) {
	return (root + 1) << 1;
}

int heap_left(int root) {
	return (root << 1) + 1;
}

void heap_delete(Heap *self) {}

T heap_get(Heap *self, int index) {
	Vector *v = self->vec;
	return v->get(v, index);
}

///// MaxHeap
void maxheap_heapify(Heap *heap, int root) {
	Vector *v = heap->vec;
	int max = root;
	
	if(heap_left(root) < heap->heapsize && v->compare(v, max, heap_left(root)) < 0) { 
		max = heap_left(root);
	}
	if(heap_right(root) < heap->heapsize && v->compare(v, max, heap_right(root)) < 0) {
		max = heap_right(root);
	}
	
	if(max != root) {
		v->swap(v, root, max);
		maxheap_heapify(heap, max);
	}
}

T maxheap_pop(Heap *self) {
	T element = self->get(self, 0);
	self->heapsize--;
	self->vec->swap(self->vec, 0, self->heapsize);
	maxheap_heapify(self, 0);
	
	return element;
}

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

Heap maxheap_from(Vector *v) {
	Heap h = (Heap) {
		.get = heap_get,
		.pop = maxheap_pop,
		.push = maxheap_push,
		.delete = heap_delete,
		.heapsize = v->size,
		.vec = v,
	};
	
	for(int i = v->size / 2; i >= 0; i--) {
		maxheap_heapify(&h, i);
	}
	
	return h;
}

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
void minheap_heapify(Heap *heap, int root) {
	Vector *v = heap->vec;
	int min = root;
	
	if(heap_left(root) < heap->heapsize && v->compare(v, min, heap_left(root)) > 0) { 
		min = heap_left(root);
	}
	if(heap_right(root) < heap->heapsize && v->compare(v, min, heap_right(root)) > 0) {
		min = heap_right(root);
	}
	
	if(min != root) {
		v->swap(v, root, min);
		minheap_heapify(heap, min);
	}
}

T minheap_pop(Heap *self) {
	T element = self->get(self, 0);
	self->heapsize--;
	self->vec->swap(self->vec, 0, self->heapsize);
	minheap_heapify(self, 0);
	
	return element;
}

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

Heap minheap_from(Vector *v) {
	Heap h = (Heap) {
		.get = heap_get,
		.pop = minheap_pop,
		.push = minheap_push,
		.delete = heap_delete,
		.heapsize = v->size,
		.vec = v,
	};
	
	for(int i = v->size / 2; i >= 0; i--) {
		minheap_heapify(&h, i);
	}
	
	return h;
}

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

