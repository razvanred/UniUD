#include <stdio.h>
#include <stdlib.h>

void readline(char s[], unsigned int size);
void parseArray(int **v, unsigned int *size, char s[]);
int partition(int array[], int lower, int upper);
void swap(int array[], int first, int second);
int median_of_medians_select(int array[], int lower, int upper, int k);
int median_of_medians(int array[], int lower, int upper);
int cubic_selection(int array[], int lower, int upper, int k);

int main(int argc, char** argv) {
	int *v;
    unsigned int size; 
    char str[10000];
    unsigned int k;
    //leggo l'array, la posizione, ed eseguo la ricerca
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", median_of_medians_select(v, 0, size - 1, k - 1));
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

void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
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

