#include <stdio.h>
#include <stdlib.h>

#define INPUT_SIZE 10000

extern void readLine(char s[const], int sLength);
extern int *stringAlloc(int *length, int maxLength, char const s[]);
extern int quickSelect(int v[], int vLength, int k);
extern int heapSelect(int v[], int vLength, int k);
extern int mdmSelect(int v[], int vLength, int k);

int main(void) {
	char s[INPUT_SIZE];

	readLine(s, INPUT_SIZE);
	int vLength;
	int *const v = stringAlloc(&vLength, INPUT_SIZE, s);
	readLine(s, INPUT_SIZE);
	int kvLength;
	int *const kv = stringAlloc(&kvLength, 1, s);
	int const k = kv[0]-1;
	free(kv);

	int r;

	// r = quickSelect(v, vLength, k);
	// r = heapSelect(v, vLength, k);
	r = mdmSelect(v, vLength, k);

	printf("%d\n", v[r]);

	free(v);
	return 0;
}
