#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_SIZE 4096

extern void readLine(char s[], int sLength);

int min3(int const a, int const b, int const c) {
	return (a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c));
}

/*
 * naive implementation
 * exponential
 */
int editDistanceRic(char const *const s1, char const *const s2) {
	if(*s1 == 0 && *s2 == 0) {
		return 0;
	}
	if(*s1 == 0) {
		return 1+editDistanceRic(s1, s2+1);
	}
	if(*s2 == 0) {
		return 1+editDistanceRic(s1+1, s2);
	}

	if(*s1 == *s2) {
		return editDistanceRic(s1+1, s2+1);
	}

	return 1+min3(editDistanceRic(s1, s2+1),        // insert
								editDistanceRic(s1+1, s2),        // delete
								editDistanceRic(s1+1, s2+1)); // substitute
}

/*
 * uses memoization
 * iterative bottom up implementation
 * iterates over a |s1|*|s2| 2d array, cost Θ(|s1|*|s2|)
 */
int editDistance(char const *const s1, char const *const s2) {
	int const width = (int)strlen(s1)+1;
	int const height = (int)strlen(s2)+1;
	int *const memos = calloc((unsigned)width*(unsigned)height, sizeof(int));

	for(int y = 0; y < height; ++y) {
		memos[width-1 /*]*/ +width* /*[*/ y] = height-1-y;
	}
	for(int x = 0; x < width-1; ++x) {
		memos[x /*]*/ +width* /*[*/ (height-1)] = width-1-x;
	}
	/*
	 . . . . . 2
	 . . . . . 1
	 5 4 3 2 1 0
	*/

	for(int y = height-2; y >= 0; --y) {
		for(int x = width-2; x >= 0; --x) {
			if(s1[x] == s2[y]) {
				memos[x /*]*/ +width* /*[*/ y] = memos[x+1 /*]*/ +width* /*[*/ (y+1)];
			} else {
				memos[x /*]*/ +width* /*[*/ y] = 1+min3(memos[x+1 /*]*/ +width* /*[*/ y],      // delete
																								memos[x /*]*/ +width* /*[*/ (y+1)],    // insert
																								memos[x+1 /*]*/ +width* /*[*/ (y+1)]); // substitute
			}
		}
	}
	int const res = memos[0];
	free(memos);
	return res;
}

int main(void) {
	char s1[INPUT_SIZE], s2[INPUT_SIZE];

	readLine(s1, INPUT_SIZE);
	readLine(s2, INPUT_SIZE);

	// printf("%d\n", editDistanceRic(s1, s2));
	printf("%d\n", editDistance(s1, s2));

	return 0;
}
