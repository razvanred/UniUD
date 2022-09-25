#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_SIZE 10000

extern void readLine(char s[], int sLength);

int periodQuad(char const s[const], int const sLength) {
	int f = 0, i;

	do {
		++f;
		for(i = 0; i < sLength && s[i] == s[i%f]; ++i);
	} while(f < sLength && i < sLength);
	return f;
}

int longestEdge(char const s[const], int const sLength) {
	// r[i] = longest edge of s[0...i]
	// r[i] = r(i + 1)
	int *const r = malloc((unsigned)sLength*sizeof(int));
	// r(1) = 0
	r[0] = 0;
	// for each substring s[0...i]
	// from r(2) to r(sLength)
	for(int i = 1; i < sLength; ++i) {
		// computing r(i + 1)
		int j;
		// from r(i) to r(1) search compatible border
		for(j = i-1; j >= 0 && s[i] != s[r[j]]; j = r[j]-1);
		if(j < 0) {
			r[i] = 0;
		} else {
			// given that r(k) > r(r(k))
			r[i] = r[j]+1;
		}
	}
	int const t = r[sLength-1];
	free(r);
	return t;
}

int period(char const s[const], int const sLength) {
	return sLength-longestEdge(s, sLength);
}

int main(void) {
	char s[INPUT_SIZE];
	readLine(s, INPUT_SIZE);
	int r;

	// r = periodQuad(s, strlen(s));
	r = period(s, (int)strlen(s));

	printf("%d\n", r);

	return 0;
}
