#include <stdlib.h>

// Le estensioni GNU permettono di usare in C un equivalente dei generici di C++ con monomorfizzazione
// Non potendo controllare il comando di compilazione del server rinuncio ai generics e uso un typedef
// Ci sarebbe un'altra opzione utilizzando gli indirizzi void*; ma o genera warning del compilatore, o
// costringe ad usare la memoria dinamica quando non è necessario
// Nell'implementazione userò direttamente int, ipotizzando di specializzare la classe astratta.
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

//distruttore di default (vuoto)
void vector_delete(Vector *self) {}

///// IntVector
//distruttore di IntVector, deve solo liberare l'array di dati, esternamente si liberera la struttura
void int_vector_delete(Vector *self) {
	free(self->data);
}

//la comparazione in IntVector è solo una comparazione tra elementi nell'array
int int_vector_compare(Vector *self, int left, int right) {
	int *data = (int*)self->data;
	return data[left] - data[right];
}

//lo scambio in IntVector è solo uno scambio tra elementi nell'array
void int_vector_swap(Vector *self, int left, int right) {
	int *data = (int*)self->data;
	int temp = data[left];
	data[left] = data[right];
	data[right] = temp;
}

//setta un elemento dell'array
void int_vector_set(Vector *self, int index, int element) {
	int *data = (int*)self->data;
	data[index] = element;
}

//restituisce un elemento dell'array
int int_vector_get(Vector *self, int index) {
	T *data = (T*)self->data;
	return data[index];
}

//costruisce un IntVector con un proprio array
Vector int_vector_new(int size) {
	//alloco l'array da inserire nel blocco nella struttura
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

//costruisce un IntVector su un array esterno
Vector int_vector_from(int *data, int size) {
	return (Vector) {
		.compare = int_vector_compare,
		.swap = int_vector_swap,
		.set = int_vector_set,
		.get = int_vector_get,
		.delete = vector_delete, //distruttore vuoto (non devo occuparmi dell'array che non ho allocato)
		.size = size,
		.data = data,
	};
}


///// IndexVector
//blocco di dati di un vettore di indici
//contiene un riferimento ad un array esterno ed uno ad un array di indici dell'array esterno
typedef struct {
	int *ref;
	int *data;
} IndexData;

//il distruttore libera l'array di indici e la sruttura del blocco dati, la struttura di IndexVector si libera esternamente
void index_vector_delete(Vector *self) {
	free(((IndexData*)self->data)->data);
	free(self->data);
}

//la comparazione tra elementi di IndexVector compara gli elementi dell'array esterno agli indici trovati nell'array di indici
int index_vector_compare(Vector *self, int left, int right) {
	int *data = ((IndexData*)self->data)->data;
	int *ref = ((IndexData*)self->data)->ref;
	return ref[data[left]] - ref[data[right]];
}

//lo scambio tra elementi di IndexVector scambia gli elementi dell'array di indici
void index_vector_swap(Vector *self, int left, int right) {
	IndexData *data = (IndexData*)self->data;
	int temp = data->data[left];
	data->data[left] = data->data[right];
	data->data[right] = temp;
}

//setta un elemento dell'array di indici
void index_vector_set(Vector *self, int index, int element) {
	IndexData *data = (IndexData*)self->data;
	data->data[index] = element;
}

//restituisce un elemento dal array di indici
int index_vector_get(Vector *self, int index) {
	IndexData *data = (IndexData*)self->data;
	return data->data[index];
}

//costruttore di IndexVector. richiede un indirizzo all'array esterno
Vector index_vector_new(int size, int *ref) {
	//alloco e setto il blocco dati da inserire nella struttura
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

