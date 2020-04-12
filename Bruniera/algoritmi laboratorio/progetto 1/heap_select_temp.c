#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
void swap(int array[], int first, int second);
int heap_select(int array[], int lower, int upper, int k);

struct Heap {
	int* array;
	int heapsize;
	int max_size;
	bool borrowed;
};

struct Option {
	bool valid;
	int element;
};

struct Heap* new_maxheap(int size);                    //crea una maxheap vuota di dimensione massima size
struct Heap* build_maxheap(int array[], int size);     //trasforma il vettore in un vettore di maxheap e lo da in prestito ad una maxheap completa
void heapify_maxheap(struct Heap* heap, int root);     //trasforma l'elemento root in una radice di maxheap, se i suoi figli sono radici di maxheap (merge in root di due maxheap)
struct Heap* new_minheap(int size);                    //crea una maxheap vuota di dimensione massima size
struct Heap* build_minheap(int array[], int size);     //trasforma il vettore in un vettore di maxheap e lo da in prestito ad una maxheap completa
void heapify_minheap(struct Heap* heap, int root);     //trasforma l'elemento root in una radice di maxheap, se i suoi figli sono radici di maxheap (merge in root di due maxheap)
int get(struct Heap* heap, int index);                 //getter per compatibilità (non aggirare)
void set(struct Heap* heap, int index, int key);       //setter per compatibilità (non aggirare)
int parent(int root);                                  //dato un l'indice di un nodo restituisce quello del genitore
int right_child(int root);                             //dato un l'indice di un nodo restituisce quello del figlio di destra
int left_child(int root);                              //dato un l'indice di un nodo restituisce quello del figlio di sinistra
void swap_heap(struct Heap* heap, int a, int b);    //scambia due elementi di una maxheap
bool push_maxheap(struct Heap* heap, int key);         //inserisce l'elemento key in una maxheap
struct Option pop_maxheap(struct Heap* heap);          //estrae la testa della maxheap e la mette in coda all'ultima foglia. Viene restituita in una struttura statica che conterra valid==false se la heap è vuota e non modificherà la dimensione
bool push_minheap(struct Heap* heap, int key);         //inserisce l'elemento key in una maxheap
struct Option pop_minheap(struct Heap* heap);          //estrae la testa della maxheap e la mette in coda all'ultima foglia. Viene restituita in una struttura statica che conterra valid==false se la heap è vuota e non modificherà la dimensione
void delete(struct Heap* heap);                        //libera la memoria occupata dalla maxheap. Se il vettore è in prestito non lo libera


int main(int argc, char** argv) {
	int *v;
    unsigned int size; 
    char str[10000];
    unsigned int k;
    
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", heap_select(v, 0, size - 1, k - 1));
}

void readline(char s[], unsigned int size) {
  unsigned int i= 0;
  char c;
  size--;
  for(c= getchar(); c != '\n';) {
    if(i < size) {
      s[i]= c;
      i++;
    }
    c= getchar();
  }
  if(i >= 0){
    s[i]= 0;
    i--;
  }
  
  while(s[i] == ' ') {
    s[i] = 0;
    i--;
  }
}

void parseArray(int **v, unsigned int *size, char s[]) {
  *size= 0;
  for(unsigned int i= 0; s[i] != 0; i++) {
    if(s[i] == ' ')
      (*size)++;
  }
  (*size)++;
  *v= malloc(*size * sizeof(int));
  for(unsigned int i= 0; i < *size; i++) {
    sscanf(s, "%d", &((*v)[i]));
    for(; *s != 0 && *s != ' '; s+= sizeof(char));
    s+= sizeof(char);
  }
}

void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

int heap_select(int array[], int lower, int upper, int k) {
	return 0;
}

struct Heap* new_maxheap(int size) {
	struct Heap* heap = malloc(sizeof(struct Heap));
	
	heap->heapsize = 0;
	heap->array = malloc(sizeof(int) * size);
	heap->borrowed = false;
	heap->max_size = size;
	
	return heap;
}

struct Heap* build_maxheap(int array[], int size) {
	struct Heap* heap = malloc(sizeof(struct Heap));
	
	heap->heapsize = size;
	heap->array = array;
	heap->borrowed = true;
	heap->max_size = size;
	
	for(int i = size / 2; i >= 1; i--) {
		heapify_maxheap(heap, i);
	}
	
	return heap;
}

//////////////////////////////////////////////// non aggirare getter e setter
int get(struct Heap* heap, int index) {            //getter, nasconde l'accesso agli attributi e risolve l'indice traslato
	return heap->array[index - 1];
}

void set(struct Heap* heap, int index, int key) {  //setter, nasconde l'accesso agli attributi e risolve l'indice traslato
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

void swap_heap(struct Heap* heap, int a, int b) {  //scambia due elementi di maxheap utilizzando get() e set(), quindi accetta indici che partono da 1
	int temp = get(heap, a);
	set(heap, a, get(heap, b));
	set(heap, b, temp);
}

void heapify_maxheap(struct Heap* heap, int root) {
	int max = root;                                                                              //seleziono il massimo tra root ed i due figli (se esistono)
	if(left_child(root) <= heap->heapsize && get(heap, max) < get(heap, left_child(root))) { 
		max = left_child(root);
	}
	if(right_child(root) <= heap->heapsize && get(heap, max) < get(heap, right_child(root))) {
		max = right_child(root);
	}
	
	if(max != root) {                                                                            //se uno dei figli è maggiore di root li scambio e ricorro sulla heap di cui il figlio era radice
		swap_heap(heap, root, max);
		heapify_maxheap(heap, max);
	}
}

void delete(struct Heap* heap){
	if(!heap->borrowed){                //se l'array non è stato istanziato dalla heap non lo libera
		free(heap->array);
	}
	free(heap);                         //libera la struttura della heap
}

struct Option pop_maxheap(struct Heap* heap) {
	struct Option option = {false, get(heap, 1)}; //inserisco il primo elemento in una struttura statica (a prescindere che sia valido o meno)
	
	if(heap->heapsize > 0) {                      //se ci sono valori
		swap_heap(heap, 1, heap->heapsize);    //porto l'ultima foglia in prima posizione (e il primo elemento al posto dell'ultinma foglia)
		heap->heapsize--;
		heapify_maxheap(heap, 1);                         //ricostruisco la heap
		option.valid = true;                      //se c'erano elementi il valore nella struttura è valido
	}
	
	return option;                                //restituisco la struttura col valore. se non è valido il valore il campo `valid` conterra ancora false
}

bool push_maxheap(struct Heap* heap, int key) {
	if(heap->heapsize >= heap->max_size) {          //controllo che non sia già piena
		return false;
	}
	
	int i = heap->heapsize + 1;                     //incremento la heapsize e inserisco il valore come ultima foglia
	heap->heapsize++;
	set(heap, heap->heapsize, key);
	
	while(i > 1 && (key > get(heap, parent(i)))) {  //finche l'elemento è più grande del suo genitore (e non è ancora arrivato in cima alla heap)
		swap_heap(heap, i, parent(i));           //lo scambio col suo genitore
		i = parent(i);
	}
	
	return true;
}

