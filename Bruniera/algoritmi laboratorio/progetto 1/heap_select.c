#include <stdio.h>
#include <stdlib.h>
#include "heap.h"

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
int heap_select(int array[], int size, int k);

int main(int argc, char** argv) {
	int *v;
    unsigned int size; 
    char str[10000];
    unsigned int k;
    //leggo l'array, la posizione, ed eseguo la ricerca
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", heap_select(v, size, k - 1));
    return 0;
}

int heap_select(int array[], int size, int k) {
	int iter;
	//creo le strutture per i due vettori, uno di dati (primary) e l'altro di riferimenti (secondary).
	//e per le rispettive due heap, una popolata (hep) e l'altra vuota (ref)
	Vector primary = int_vector_from(array, size);
	Vector secondary;
	Heap heap;
	Heap ref;
	
	//se k è nella prima metà del vettore uso le MinHeap, altrimenti uso le MaxHeap.
	//la struttura è indifferente, cambia come sarà inizializzata
	if(k <= size / 2) {
		//il numero delle iterazioni corrisponde alla posizione k
		iter = k;
		secondary = index_vector_new(iter + 1, array);
		heap = minheap_from(&primary);
		ref = minheap_with(&secondary);
	} else {
		//dato che si cerca l'elemento dalla parte finale del vettore ordinato,
		//il numero di iterazioni NON corrisponde alla posizione k
		iter = size - k - 1;
		secondary = index_vector_new(iter + 1, array);
		heap = maxheap_from(&primary);
		ref = maxheap_with(&secondary);
	}
	
	//inserisce in ref un indice al primo elemento della heap
	ref.push(&ref, 0);

	for(; iter > 0; iter--) {
		//estrae la radice di ref
		int temp = ref.pop(&ref);
		//se esistono nella heap, aggiunge a ref gli indici dei figli del nodo in heap puntato dalla radice di ref
		if(heap_left(temp) < heap.heapsize) {
			ref.push(&ref, heap_left(temp));
		}
		if(heap_right(temp) < heap.heapsize) {
			ref.push(&ref, heap_right(temp));
		}
	}
	//dopo `iter` iterazioni, la radice di ref conterrà l'indice dell'elemento selezionato
	int ret = primary.get(&primary, secondary.get(&secondary, 0));
	
	//Avendo scritto l'implementazione dei distruttori, so che quelli commentati sono superflui
	//le strutture sono statiche quindi non devono essere liberate
	/*heap.delete(&heap);
	ref.delete(&ref);
	primary.delete(&primary);*/
	secondary.delete(&secondary);
	
	return ret;
}

//leggo la riga come stringa
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

//produco un vettore dalla stringa
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

