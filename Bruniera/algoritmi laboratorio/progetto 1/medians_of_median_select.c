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
    
	readline(str, 10000);
    parseArray(&v, &size, str);
    scanf("%d", &k);
    printf("%d\n", median_of_medians_select(v, 0, size - 1, k - 1));
}

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
    if(lower == upper) {
        return array[upper];
    }
    int times = (upper - lower) / 5;
    int last = upper;
    
    cubic_selection(array, lower + (5 * times), upper,  upper - (((upper - lower) % 5) / 2));
    swap(array,upper - (((upper - lower) % 5) / 2), last);
    last--;
    for(int i = times - 1; i >= 0; i--) {
        cubic_selection(array, lower + (5 * i), lower + (5 * i) + 4, lower + (5 * i) + 2);
        swap(array, lower + (5 * i) + 2, last);
        last--;
    }
    
    return median_of_medians(array, upper - times, upper);
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

int partition(int array[], int lower, int upper) {
	median_of_medians(array, lower, upper);
	int break_even = lower - 1;
	for(int i = lower; i <= upper; i++){
		if(array[i] <= array[upper]){
			break_even++;
			swap(array, break_even, i);
		}
	}	
	
	return break_even;
}

void swap(int array[], int first, int second) {
	int temp = array[first];
	array[first] = array[second];
	array[second] = temp;
}

int median_of_medians_select(int array[], int lower, int upper, int k) {
	if(lower > k || upper < k) {
		return -1;
	}
	int break_even = partition(array, lower, upper);
	
	if(k == break_even) {
		return array[k];
	} else if(k > break_even) {
		return median_of_medians_select(array, break_even + 1, upper, k);
	} else {
		return median_of_medians_select(array, lower, break_even - 1, k);
	}
}

