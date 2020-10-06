#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "heap.h"

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
int partition(int array[], int lower, int upper);
void swap(int array[], int first, int second);
int quick_select(int array[], int lower, int upper, int k);
int heap_select(int array[], int size, int k);
int heap_select_standardize(int array[], int lower, int upper, int k);
int median_of_medians_select(int array[], int lower, int upper, int k);
int median_of_medians(int array[], int lower, int upper);
int cubic_selection(int array[], int lower, int upper, int k);
void timespec_dif(const struct timespec *a, const struct timespec *b, struct timespec *result);
void timespec_sum(const struct timespec *a, const struct timespec *b, struct timespec *result);
void timespec_mul(const struct timespec *a,  int b, struct timespec *result);
void timespec_div(const struct timespec *a,  int b, struct timespec *result);
void timespec_getres(struct timespec *res);
void try_select(int (* select)(int*, int, int, int), int array[], int n, int k, struct timespec threshold, struct timespec time_array[]);

int main(int argc, char** argv) {
    int *array;
    struct timespec result, threshold, *time_array;
    //seed random
    srand(time(NULL));
	//calcolo la soglia per avere un errore massimo del 0.01% dalla risoluzione reale
	timespec_getres(&result);
	//printf("%ld.%09ld\n", result.tv_sec, result.tv_nsec);
	timespec_mul(&result, 100, &threshold);
	//printf("%ld.%09ld\n\n", threshold.tv_sec, threshold.tv_nsec);
	
	for(int order = 100; order > 0; order *= 10) {
		for(int n = order, c = 0; n <= 5000000 && c < 9; n += order, c++) {
			//alloco memoria
			if((array = malloc(n * sizeof(int))) == 0) {
				exit(1);
			}
			if((time_array = malloc(100 * sizeof(struct timespec))) == 0) {
				exit(1);
			}
			//print N K
			printf("%d %d ", n, n / 4);
			
			//inizio con quick_select
			try_select(&quick_select, array, n, n/4, threshold, time_array);
			
		    
		    //ripeto per heap_select
			try_select(&heap_select_standardize, array, n, n/4, threshold, time_array);
		    
		    //ripeto per median_of_medians_select
			try_select(&median_of_medians_select, array, n, n/4, threshold, time_array);
		    
		    //a capo
		    puts("");
		    //libero gli array
		    free(array);
		    free(time_array);
		}
	}
}

void try_select(int (* select)(int*, int, int, int), int array[], int n, int k, struct timespec threshold, struct timespec time_array[]) {
    struct timespec start, end, result, sum;
    int counter;
    long double deviation;
	/*resetto il contatore*/
	sum.tv_nsec = 0;
	sum.tv_sec = 0;
	for(int j = 0; j < 100; j++) {
		/*resetto altri contatori*/
		counter = 0;
		/*randomizzo l-array*/
		for(int i = 0; i < n ; i++){
    		array[i] = rand();
		}
		/*parte il timer e lancio la funzione*/
		clock_gettime(CLOCK_MONOTONIC, &start);
		do {
			select(array, 0, n - 1, k);
			/*fermo il timer e calcolo la differenza*/
			clock_gettime(CLOCK_MONOTONIC, &end);
			timespec_dif(&end, &start, &result);
			/*aumento i contatori*/
			counter++;
			/*ripeto se ci ha messo troppo poco*/
	    } while(result.tv_sec < threshold.tv_sec || (result.tv_sec == threshold.tv_sec && result.tv_nsec < threshold.tv_nsec));
	    /*finito calcolo il tempo medio delle ripetizioni*/
	    start = result;
	    timespec_div(&start, counter, &result);
	    /*incremento il contatore dei campioni e salvo il campione*/
	    time_array[j] = result;
	    start = sum;
	    timespec_sum(&start, &result, &sum);
	}
	/*calcolo la media dei campioni*/
	timespec_div(&sum, 100, &result);
	deviation = 0;
	/*calcolo la somma degli scarti al quadrato*/
	for(int j = 0; j < 100; j++) {
		timespec_dif(&time_array[j], &result, &sum);
		long double partial = (long double) sum.tv_nsec;
		partial = partial / 1000000000;
		partial += sum.tv_sec;
		partial = partial * partial;
		deviation += partial;
	}
	/*stampo i valori*/
    printf("%ld.%09ld %Lg ", result.tv_sec, result.tv_nsec, sqrt(deviation / 100));
}

int inline heap_select_standardize(int array[], int lower, int upper, int k) {
	return heap_select(array, upper + 1, k);
}

//calcolo la reale risoluzione del timer
void timespec_getres(struct timespec *res) {
	struct timespec result, start, end;
	res->tv_nsec = 0;
    res->tv_sec = 0;
    int times = 10000;
    for(int j=0; j < times; j++) {
	    clock_gettime(CLOCK_MONOTONIC, &start);
		do {
			clock_gettime(CLOCK_MONOTONIC, &end);
	    	timespec_dif(&end, &start, &result);
	    } while(result.tv_sec == 0 && result.tv_nsec == 0);
		//printf("%ld.%09ld %ld.%09ld\n", result.tv_sec, result.tv_nsec, res.tv_sec, res.tv_nsec);
	    start = *res;
		timespec_sum(&start, &result, res);
	}
	start = *res;
	timespec_div(&start, times, res);
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

int partition(int array[], int lower, int upper) {	
	//swap(array, k, upper); //swap non necessario, scelgo sempre l'ultimo come pivot
	int break_even = lower - 1;
	
	for(int i = lower; i <= upper; i++){
		//eseguo lo scambio se l'elemento è più grande dell'ultimo
		if(array[i] <= array[upper]){
			break_even++;
			swap(array, break_even, i);
		}
	}	
	
	//restituisco il pivot
	return break_even;
}

//scambio due elementi
void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

int quick_select(int array[], int lower, int upper, int k) {
	//termina se l'array è vuoto
	if(lower > k || upper < k) {
		return -1;
	}
	//partioziono l'array senza criterio
	int break_even = partition(array, lower, upper);
	
	if(k == break_even) {
		//se il pivot è la posizione che cerco restituisco l'elemento
		return array[k];
	} else if(k > break_even) {
		//altrimenti ricorro sulla partizione che contiene la posizione
		return quick_select(array, break_even + 1, upper, k);
	} else {
		return quick_select(array, lower, break_even - 1, k);
	}
	//n.b.: al termine l'elemento selezionato si trova nella posizione k
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

int median_of_medians_select(int array[], int lower, int upper, int k) {
	//termina se l'array è vuoto
	if(lower > k || upper < k) {
		return -1;
	}
	median_of_medians(array, lower, upper);
	//medians_of_medians porta il mediano in ultima posizione, pronto per il partizionamento
	//partiziono l'array con il mediano dei mediani come pivot
	int break_even = partition(array, lower, upper);
	
	if(k == break_even) {
		//se il pivot è la posizione che cerco restituisco l'elemento
		return array[k];
	} else if(k > break_even) {
		//altrimenti ricorro sulla partizione che contiene la posizione
		//la partizione avrà dimensione al più n*7/10 grazie alla scelta del pivot
		return median_of_medians_select(array, break_even + 1, upper, k);
	} else {
		return median_of_medians_select(array, lower, break_even - 1, k);
	}
	//n.b.: al termine l'elemento selezionato si trova nella posizione k
}

//selezione inefficiente basata su selection_sort
int cubic_selection(int array[], int lower, int upper, int index) {
	int selected;// = lower;
	
	for(int i = lower; i <= index; i++) {
		selected = i;
		for(int j = i + 1; j <= upper; j++) {
			if(array[j] < array[selected]) {
				selected = j;
			}
		}
		swap(array, selected, i);
	}
	
	return array[index];
}

int median_of_medians(int array[], int lower, int upper) {
	//termina se l'array è singoletto
    if(lower == upper) {
        return array[upper];
    }
    int times = (upper - lower) / 5;
    int last = upper;
    
    //trovo il mediano dell'ultima partizione e lo metto in ultima posizione (perché potrebbe avere meno di 5 elementi)
    cubic_selection(array, lower + (5 * times), upper,  upper - (((upper - lower) % 5) / 2));
    swap(array,upper - (((upper - lower) % 5) / 2), last);
    last--;
    
    //trovo il mediano delle restanti partizioni (in ordine decrescente)
    for(int i = times - 1; i >= 0; i--) {
        cubic_selection(array, lower + (5 * i), lower + (5 * i) + 4, lower + (5 * i) + 2);
        //sposto i mediani alla fine dell'array
        swap(array, lower + (5 * i) + 2, last);
        last--;
    }
    
    //trovo il mediano tra i mediani (accumulati in fondo all'array)
    median_of_medians_select(array, upper - times, upper, upper - (times / 2));
    //porto il mediano in ultima posizione
	swap(array, upper - (times / 2), upper);
	//restituisco il mediano
	return array[upper];
}


//funzione per la sottrazione tra timespec adattata dalla macro timeval_diff_macro in sys/time.h
void timespec_dif(const struct timespec *a, const struct timespec *b, struct timespec *result) {
    result->tv_sec = a->tv_sec - b->tv_sec;
    result->tv_nsec = a->tv_nsec - b->tv_nsec;
    if (result->tv_nsec < 0) {
        result->tv_sec--;
        result->tv_nsec += 1000000000L;
    }
}

//somma tra timespec adattata da timespec_dif
void timespec_sum(const struct timespec *a, const struct timespec *b, struct timespec *result) {
    result->tv_sec  = a->tv_sec + b->tv_sec;
    result->tv_nsec = a->tv_nsec + b->tv_nsec;
    if (result->tv_nsec > 1000000000L) {
        result->tv_sec++;
        result->tv_nsec -= 1000000000L;
    }
}

//moltiplicazione per intero di timespec. (a+b/1E9)*c = c*a+cb/1E9
void timespec_mul(const struct timespec *a, int b, struct timespec *result) {
    long long temp = (long long) b * (long long) a->tv_nsec;
    result->tv_sec = (a->tv_sec * b) + (temp / 1000000000LL);
    result->tv_nsec = temp % 1000000000LL;
}

//divisione per intero di timespec. (a+b/1E9)/c = a/c+b/cE9
void timespec_div(const struct timespec *a, int b, struct timespec *result) {
    long long temp = (((long long) (a->tv_sec % b) * 1000000000LL) + (long long) a->tv_nsec) / (long long) b;
    result->tv_sec = (a->tv_sec / b) + (temp / 1000000000LL);
    result->tv_nsec = temp % 1000000000LL;
}
