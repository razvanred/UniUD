#include <stdlib.h>

// Le estensioni GNU permettono di usare in C un equivalente dei generici di C++ con monomorfizzazione
// Non potendo controllare il comando di compilazione del server rinuncio ai generics e uso un typedef
// Ci sarebbe un'altra opzione utilizzando gli indirizzi void*; ma o genera warning del compilatore, o
// costringe ad usare la memoria dinamica quando non è necessario
typedef int T;

struct Vector {
	int (*compare)(struct Vector*, int, int); 
	void (*swap)(struct Vector*, int, int);
	void (*set)(struct Vector*, int, T);
	T (*get)(struct Vector*, int);
	void (*delete)(struct Vector*);
	int size;
	void *data;
};

typedef struct Vector Vector;

void vector_delete(Vector *self) {}

///// IntVector
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

int int_vector_get(Vector *self, int index) {
	T *data = (T*)self->data;
	return data[index];
}

Vector int_vector_new(int size) {
	int *data = malloc(sizeof(int[size]));
	return (Vector) {
		.compare = int_vector_compare,
		.swap = int_vector_swap,
		.set = int_vector_set,
		.get = int_vector_get,
		.delete = int_vector_delete,
		.size = size,
		.data = data,
	};
}

Vector int_vector_from(int *data, int size) {
	return (Vector) {
		.compare = int_vector_compare,
		.swap = int_vector_swap,
		.set = int_vector_set,
		.get = int_vector_get,
		.delete = vector_delete,
		.size = size,
		.data = data,
	};
}


///// IndexVector
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

int index_vector_get(Vector *self, int index) {
	IndexData *data = (IndexData*)self->data;
	return data->data[index];
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
		.set = index_vector_set,
		.get = index_vector_get,
		.delete = index_vector_delete,
		.size = size,
		.data = data,
	};
}

