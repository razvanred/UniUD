#include <stdlib.h>

struct Vector {
	int (*compare)(struct Vector*, int, int); 
	void (*swap)(struct Vector*, int, int);
	void (*set)(struct Vector*, int, void*);
	void* (*get)(struct Vector*, int);
	int size;
	void *data;
};

typedef struct Vector Vector;

///// int_vector
void int_vector_delete(Vector *self) {
	free(self->data);
}

int int_vector_compare(Vector *self, int left, int right) {
	int *data = (int*)self->data;
	return data[left] - data[right];
}

void int_vector_swap(Vector *self, int left, int right) {
	int *data = (int*)self->data;
	int temp = data[left];
	data[left] = data[right];
	data[right] = temp;
}

void int_vector_set(Vector *self, int index, int element) {
	int *data = (int*)self->data;
	data[index] = element;
}

void* int_vector_get(Vector *self, int index) {
	int *data = (int*)self->data;
	return (void*)data[index];
}

Vector int_vector_new(int size) {
	int *data = malloc(sizeof(int[size]));
	return (Vector) {
		.compare = int_vector_compare,
		.swap = int_vector_swap,
		.set = (void (*)(struct Vector*, int, void*))int_vector_set,
		.get = int_vector_get,
		.size = size,
		.data = data,
	};
}


///// index_vector
typedef struct {
	int *ref;
	int *data;
} IndexData;

void index_vector_delete(Vector *self) {
	free(((IndexData*)self->data)->data);
	free(self->data);
}

int index_vector_compare(Vector *self, int left, int right) {
	int *data = ((IndexData*)self->data)->data;
	int *ref = ((IndexData*)self->data)->ref;
	return ref[data[left]] - ref[data[right]];
}

void index_vector_swap(Vector *self, int left, int right) {
	IndexData *data = (IndexData*)self->data;
	int temp = data->data[left];
	data->data[left] = data->data[right];
	data->data[right] = temp;
}

void index_vector_set(Vector *self, int index, int element) {
	IndexData *data = (IndexData*)self->data;
	data->data[index] = element;
}

void* index_vector_get(Vector *self, int index) {
	IndexData *data = (IndexData*)self->data;
	return (void*)data->data[index];
}

Vector index_vector_new(int size, int *ref) {
	IndexData *data = malloc(sizeof(IndexData));
	*data = (IndexData) {
		.ref = ref,
		.data = malloc(sizeof(int[size])),
	};
	
	return (Vector) {
		.compare = index_vector_compare,
		.swap = index_vector_swap,
		.set = (void (*)(struct Vector*, int, void*))index_vector_set,
		.get = index_vector_get,
		.size = size,
		.data = data,
	};
}

