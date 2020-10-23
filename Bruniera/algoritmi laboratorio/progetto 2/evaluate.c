#include <time.h>
#include <math.h>
#include "trees.h"

struct clear_stack {
	void* tree;
	struct clear_stack *next;
};

typedef struct clear_stack clear_stack;

void timespec_dif(const struct timespec *a, const struct timespec *b, struct timespec *result);
void timespec_sum(const struct timespec *a, const struct timespec *b, struct timespec *result);
void timespec_mul(const struct timespec *a,  int b, struct timespec *result);
void timespec_div(const struct timespec *a,  int b, struct timespec *result);
void timespec_getres(struct timespec *res);
void try_tree(void (* insert)(long, char*, void**), char *(* find)(void*, long), void (* clear)(void*), void  **tree, int n, struct timespec threshold);
void clear_all(void (* clear)(void*), clear_stack *stack);
long rand_plus();

int main(int argc, char** argv) {
	srand(time(NULL));
	
    Node *tree;
    AvlNode *avl_tree;
    RbtNode *rbt_tree;
	tree = NULL;
	avl_tree = NULL;
	rbt_tree = NULL;
	
	struct timespec result, threshold;
    //seed random
    srand(time(NULL));
	//calcolo la soglia per avere un errore massimo del 0.01% dalla risoluzione reale
	timespec_getres(&result);
	//printf("%ld.%09ld\n", result.tv_sec, result.tv_nsec);
	timespec_mul(&result, 100, &threshold);
	//printf("%ld.%09ld\n\n", threshold.tv_sec, threshold.tv_nsec);
	
	for(int order = 1000; order <= 100000; order *= 10) {
		for(int n = order, c = 0; c < 18 && n <= 300000; n += order / 2, c++) {
			printf("%d ", n);
			
			try_tree(&insert, &find, &clear, &tree, n, threshold);
			
			try_tree(&avl_insert, &avl_find, &avl_clear, &avl_tree, n, threshold);
			
			try_tree(&rbt_insert, &rbt_find, &rbt_clear, &rbt_tree, n, threshold);
			
			puts("");
		}
	}
    
    return 0;
}

void try_tree(void (* insert)(long, char*, void**), char *(* find)(void*, long), void (* clear)(void*), void  **tree, int n, struct timespec threshold) {
    struct timespec start, end, result, sum, time_array[100];
    int counter;
    long key;
    long double deviation;
    char placeholder[] = "a";
	clear_stack *head, *temp;
	//resetto il contatore
	sum.tv_nsec = 0;
	sum.tv_sec = 0;
	for(int j = 0; j < 100; j++) {
		//resetto contatore e inizializzo la lista
		counter = 0;
		head = NULL;
		//parte il timer e lancio la funzione
		clock_gettime(CLOCK_MONOTONIC, &start);
		do {
			for(int i = 0; i < n; i++) {
				key = rand_plus();
				if(find(*tree, key) == NULL) {
					insert(key, placeholder, tree);
				}
			}
			//inserisco l'albero in uno stack di alberi da liberare
			temp = malloc(sizeof(clear_stack));
			temp->next = head;
			head = temp;
			head->tree = *tree;
			//inizializzo un nuovo albero
			*tree = NULL;
			//aumento il contatore
			counter++;
			//fermo il timer e calcolo la differenza
			clock_gettime(CLOCK_MONOTONIC, &end);
			timespec_dif(&end, &start, &result);
			//ripeto se ci ha messo troppo poco
	    } while(result.tv_sec < threshold.tv_sec || (result.tv_sec == threshold.tv_sec && result.tv_nsec < threshold.tv_nsec));
	    //finito calcolo il tempo medio delle ripetizioni
	    start = result;
	    timespec_div(&start, counter * n, &result);
	    //incremento il contatore dei campioni e salvo il campione
	    time_array[j] = result;
	    start = sum;
	    timespec_sum(&start, &result, &sum);
	    //libero tutti gli alberi accumulati
		clear_all(clear, head);
	}
	//calcolo la media dei campioni
	timespec_div(&sum, 100, &result);
	deviation = 0;
	//calcolo la somma degli scarti al quadrato
	for(int j = 0; j < 100; j++) {
		timespec_dif(&time_array[j], &result, &sum);
		long double partial = (long double) sum.tv_nsec;
		partial = partial / 1000000000;
		partial += sum.tv_sec;
		partial = partial * partial;
		deviation += partial;
	}
	//stampo i valori
    printf("%ld.%09ld %Lg ", result.tv_sec, result.tv_nsec, sqrt(deviation / 100));
}

long rand_plus() {
	long a = rand();
	long b = rand();
	return (((a * 31) * b) * 31) ^ a ^ (b << 16);
}

//libera lo stack di alberi
void clear_all(void (* clear)(void*), clear_stack *stack) {
	if (stack != NULL) {
		clear(stack->tree);
		clear_all(clear, stack->next);
		free(stack);
	}
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
