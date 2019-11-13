#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

////////////////////////////////////////////////////////////////////////
// declaration

struct MaxHeap {
	int* array;
	int heapsize;
	int max_size;
	bool borrowed;
};

struct Option {
	bool valid;
	int element;
};

struct MaxHeap* new_maxheap(int size);                    //crea una maxheap vuota di dimensione massima size
struct MaxHeap* build_maxheap(int array[], int size);     //trasforma il vettore in un vettore di maxheap e lo da in prestito ad una maxheap completa
void heapify(struct MaxHeap* heap, int root);             //trasforma l'elemento root in una radice di maxheap, se i suoi figli sono radici di maxheap (merge in root di due maxheap)
int get(struct MaxHeap* heap, int index);                 //getter per compatibilità (non aggirare)
void set(struct MaxHeap* heap, int index, int key);       //setter per compatibilità (non aggirare)
int parent(int root);                                     //dato un l'indice di un nodo restituisce quello del genitore
int right_child(int root);                                //dato un l'indice di un nodo restituisce quello del figlio di destra
int left_child(int root);                                 //dato un l'indice di un nodo restituisce quello del figlio di sinistra
void swap_maxheap(struct MaxHeap* heap, int a, int b);    //scambia due elementi di una maxheap
bool push_maxheap(struct MaxHeap* heap, int key);         //inserisce l'elemento key in una maxheap
struct Option pop_maxheap(struct MaxHeap* heap);          //estrae la testa della maxheap e la mette in coda all'ultima foglia. Viene restituita in una struttura statica che conterra valid==false se la heap è vuota e non modificherà la dimensione
void delete(struct MaxHeap* heap);                        //libera la memoria occupata dalla maxheap. Se il vettore è in prestito non lo libera
void heapsort(int v[], int size);                         //ordina un vettore con l'algoritmo heapsort. Per farlo lo da in prestito ad una maxheap



////////////////////////////////////////////////////////////////////////
// test

void println_array(int a[], int length){
	for(int i = 0; i < length; i++){
		printf("%d ", a[i]);
	}
	printf("\n");
}

int main(int argc, char** argv) {
	struct Option option;
	int i = -20;
	int v[50];
	
	struct MaxHeap* heap = new_maxheap(50);
	
	while(push_maxheap(heap, i)) {
		i++;
	}
	
	println_array(heap->array, 50);
	
	while((option = pop_maxheap(heap)).valid) {
		printf("%d\n", option.element);
	}
	delete(heap);
	
	for(i=0; i<25; i++) {
		v[i] = i;
	}
	for(; i<50; i++) {
		v[i] = i - 50;
	}
	println_array(v, 50);
	heapsort(v, 50);
	println_array(v, 50);

	return 0;
}



////////////////////////////////////////////////////////////////////////
// implementation

struct MaxHeap* new_maxheap(int size) {
	struct MaxHeap* heap = malloc(sizeof(struct MaxHeap));
	
	heap->heapsize = 0;
	heap->array = malloc(sizeof(int) * size);
	heap->borrowed = false;
	heap->max_size = size;
	
	return heap;
}

struct MaxHeap* build_maxheap(int array[], int size) {
	struct MaxHeap* heap = malloc(sizeof(struct MaxHeap));
	
	heap->heapsize = size;
	heap->array = array;
	heap->borrowed = true;
	heap->max_size = size;
	
	for(int i = size / 2; i >= 1; i--) {
		heapify(heap, i);
	}
	
	return heap;
}

//////////////////////////////////////////////// non aggirare getter e setter
int get(struct MaxHeap* heap, int index) {            //getter, nasconde l'accesso agli attributi e risolve l'indice traslato
	return heap->array[index - 1];
}

void set(struct MaxHeap* heap, int index, int key) {  //setter, nasconde l'accesso agli attributi e risolve l'indice traslato
	heap->array[index - 1] = key;
}
////////////////////////////////////////////////

int parent(int root) {
	return root / 2;
}

int right_child(int root) {
	return root * 2 + 1;
}

int left_child(int root) {
	return root * 2;
}

void swap_maxheap(struct MaxHeap* heap, int a, int b) {  //scambia due elementi di maxheap utilizzando get() e set(), quindi accetta indici che partono da 1
	int temp = get(heap, a);
	set(heap, a, get(heap, b));
	set(heap, b, temp);
}

void heapify(struct MaxHeap* heap, int root) {
	int max = root;                                                                              //seleziono il massimo tra root ed i due figli (se esistono)
	if(left_child(root) <= heap->heapsize && get(heap, max) < get(heap, left_child(root))) { 
		max = left_child(root);
	}
	if(right_child(root) <= heap->heapsize && get(heap, max) < get(heap, right_child(root))) {
		max = right_child(root);
	}
	
	if(max != root) {                                                                            //se uno dei figli è maggiore di root li scambio e ricorro sulla heap di cui il figlio era radice
		swap_maxheap(heap, root, max);
		heapify(heap, max);
	}
}

void delete(struct MaxHeap* heap){
	if(!heap->borrowed){                //se l'array non è stato istanziato dalla heap non lo libera
		free(heap->array);
	}
	free(heap);                         //libera la struttura della heap
}

struct Option pop_maxheap(struct MaxHeap* heap) {
	struct Option option = {false, get(heap, 1)}; //inserisco il primo elemento in una struttura statica (a prescindere che sia valido o meno)
	
	if(heap->heapsize > 0) {                      //se ci sono valori
		swap_maxheap(heap, 1, heap->heapsize);    //porto l'ultima foglia in prima posizione (e il primo elemento al posto dell'ultinma foglia)
		heap->heapsize--;
		heapify(heap, 1);                         //ricostruisco la heap
		option.valid = true;                      //se c'erano elementi il valore nella struttura è valido
	}
	
	return option;                                //restituisco la struttura col valore. se non è valido il valore il campo `valid` conterra ancora false
}

bool push_maxheap(struct MaxHeap* heap, int key) {
	if(heap->heapsize >= heap->max_size) {          //controllo che non sia già piena
		return false;
	}
	
	int i = heap->heapsize + 1;                     //incremento la heapsize e inserisco il valore come ultima foglia
	heap->heapsize++;
	set(heap, heap->heapsize, key);
	
	while(i > 1 && (key > get(heap, parent(i)))) {  //finche l'elemento è più grande del suo genitore (e non è ancora arrivato in cima alla heap)
		swap_maxheap(heap, i, parent(i));           //lo scambio col suo genitore
		i = parent(i);
	}
	
	return true;
}

void heapsort(int v[], int size) {
	struct MaxHeap* heap = build_maxheap(v, size);  //costruisco una maxheap dal mio vettore (dandolo n prestito)
												    //la testa della heap è il valore più alto
	while(pop_maxheap(heap).valid);                 //estraggo la testa mettendola al posto dell'ultima foglia finche non finisco tutti gli elementi
	                                                //ora il vettore è ordinato perche ho inserito ogni volta il valore più grande partendo dall'ultima posizione
	delete(heap);                                   //posso eliminare la struttura, senza perdere il vettore
}
