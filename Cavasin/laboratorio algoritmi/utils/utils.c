#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <limits.h>

void lputs(char s[const]) {
	fputs(s, stdout);
}

void swap(int *const a, int *const b) {
	assert(a);
	assert(b);

	int const t = *a;
	*a = *b;
	*b = t;
}

void arraySwap(int v[const], int const i, int const k) {
	assert(i >= 0);
	assert(k >= 0);

	swap(v+i, v+k);
}

int min(int const a, int const b) {
	return a < b ? a : b;
}

int max(int const a, int const b) {
	return a > b ? a : b;
}

int intRandom(void) {
	int intBits = 0, randBits = 0;
	for(unsigned int intMax = UINT_MAX; intMax != 0; intMax >>= 1) {
		++intBits;
	}
	for(int randMax = RAND_MAX; randMax != 0; randMax >>= 1) {
		++randBits;
	}
	int r = 0;
	for(int i = 0; i < intBits/randBits+1; ++i) {
		r |= rand()<<(i*randBits);
	}
	return r;
}

int compareInt(void const *const a, void const *const b) {
	assert(a);
	assert(b);
	return *(int const *)a < *(int const *)b ? -1 : *(int const *)a == *(int const *)b ? 0 : 1;
}

int compareLongDouble(void const *const a, void const *const b) {
	assert(a);
	assert(b);
	return *(long double const *)a < *(long double const *)b ? -1 : *(long double const *)a == *(long double const *)b ? 0 : 1;
}

void arrayPrint(int const v[const], int const vLength) {
	assert(v);

	lputs("{");
	for(int i = 0; i < vLength; ++i) {
		printf(" %d", v[i]);
	}
	puts(" }");
}

void arrayRandom(int *const v, int const vLength) {
	assert(v);

	for(int i = 0; i < vLength; ++i) {
		v[i] = intRandom();
	}
}

int *randomAlloc(int const vLength) {
	int *const v = malloc((unsigned)vLength*sizeof(int));
	arrayRandom(v, vLength);
	return v;
}

int *copyAlloc(int const src[const], int const srcLength) {
	assert(src);

	int *const t = malloc((unsigned)srcLength*sizeof(int));
	if(t) {
		memcpy(t, src, (unsigned)srcLength*sizeof(int));
	}
	return t;
}

void readLine(char s[const], int sLength) {
	assert(s);

	int i = 0;
	for(int c = getchar(); c != '\n';) {
		if(i < sLength-1) {
			s[i++] = (char)c;
		}
		c = getchar();
	}
	s[i] = 0;
}

int *stringAlloc(int *const length, int const maxLength, char const s[]) {
	assert(length);
	assert(s);

	*length = 0;
	for(int pc = ' ', i = 0; s[i] != 0; ++i) {
		if(*length < maxLength && isspace(pc) && !isspace(s[i])) {
			++*length;
		}
		pc = (unsigned char)s[i];
	}
	int *const v = malloc((unsigned)*length*sizeof(int));
	for(int i = 0; i < *length; ++i) {
		int n;
		sscanf(s, "%d%n", v+i, &n); // NOLINT(cert-err34-c)
		s += n;
	}
	return v;
}

void printElapsedTime(time_t const elapsed) {
	int const hours = (int)(elapsed/3600);
	int const minutes = (int)((elapsed%3600)/60);
	int const seconds = (int)(elapsed%60);
	printf("%d hours %d minutes %d seconds", hours, minutes, seconds);
}
