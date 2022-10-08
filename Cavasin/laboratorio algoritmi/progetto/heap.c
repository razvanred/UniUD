#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "types.h"

/**
 A generic index-based heap, type and order agnostic.
 Check documentation on the Heap type too.
 This implementation lacks the additional handlers required to manipulate the values array: there's no allocator to add
 new elements or deallocator to remove them. The values here are static and used akin to a lookup table by the
 comparison function.
 This choice was made to keep the source lean.
*/

extern void lputs(char s[]);
extern void arraySwap(int v[], int i, int k);

int heapLeft(int const i) {
	return (2*i)+1;
}

int heapRight(int const i) {
	return 2*(i+1);
}

static int heapParent(int const i) {
	return (i-1)/2;
}

static int heapHeight(Heap const h) {
	int size = h.heapSize, i = 0;
	while(size >>= 1) {
		i++;
	}
	return i;
}

/**
 * Given the father of two heaps, make a bigger heap comprising the father.
 * time: O(height)=O(log n)
 * @param h pointer to a heap
 * @param i index to the father element
 */
static void heapify(Heap h, int const i) {
	int const left = heapLeft(i);
	int right = heapRight(i);
	if(left >= h.heapSize) {
		return;
	}
	if(right >= h.heapSize) {
		right = i;
	}
	int child;
	if(h.compare(h.values, h.refs[left], h.refs[right]) < 0) {
		child = left;
	} else {
		child = right;
	}
	if(h.compare(h.values, h.refs[i], h.refs[child]) < 0) {
		child = i;
	}
	if(i != child) {
		arraySwap(h.refs, i, child);
		heapify(h, child);
	}
}

/**
 * Constructs an index-based heap based on the specified values and comparison function.
 * time: Θ(size)=Θ(n)
 * @param size initial size of the heap, values from 0 to size must be populated
 * @param maxSize maximum number of elements allowed, must be <= valuesLength
 * @param values read-only data that will be available to the comparison function
 * @param valuesLength length of values
 * @param compare a comparison function
 * @return A heap
 */
Heap heapAlloc(int const size, int const maxSize, void const *const values, int const valuesLength, Comparator compare) {
	assert(values);
	assert(compare);
	assert(maxSize <= valuesLength);

	int *const refs = malloc((unsigned)maxSize*sizeof(int));
	// populating refs with indices in values
	for(int i = 0; i < size; ++i) {
		refs[i] = i;
	}
	Heap h = {.refs=refs, .refsLength=valuesLength, .maxSize=maxSize, .heapSize=size, .compare=compare, .values=values};
	for(int i = size/2-1; i >= 0; --i) {
		heapify(h, i);
	}
	return h;
}

/**
 * Deconstructs a heap. Doesn't affect values
 * @param h a heap
 */
void heapFree(Heap const h) {
	free(h.refs);
}

/**
 * Removes the element at the top of the queue. Doesn't affect values
 * @param h pointer to a heap
 * @return Index in values of the removed element, -1 if h is empty.
 */
int heapPop(Heap *h) {
	assert(h);

	if(h->heapSize <= 0) {
		return -1;
	}
	--(h->heapSize);
	arraySwap(h->refs, 0, h->heapSize);
	heapify(*h, 0);
	return h->refs[h->heapSize];
}

/**
 * Adds an element in values of index i to the queue. Doesn't affect values
 * @param h pointer to a heap
 * @param i index of the new element in values
 * @return -1 if h is full, 0 otherwise.
 */
int heapPush(Heap *h, int i) {
	assert(h);
	assert(i < h->refsLength);

	if(h->heapSize >= h->maxSize) {
		return -1;
	}
	h->refs[h->heapSize] = i;
	for(int k = h->heapSize; k > 0 && h->compare(h->values, h->refs[k], h->refs[heapParent(k)]) < 0;) {
		arraySwap(h->refs, k, heapParent(k));
		k = heapParent(k);
	}
	++(h->heapSize);
	return 0;
}

static void printHeapHelper(Heap const h, int const i, bool b[const], int const bSize) {
	assert(b);
	int t;

	t = heapRight(i);
	if(t < h.heapSize) {
		for(int j = 0; j < bSize; ++j) {
			b[j] ? lputs("│   ") : lputs("    ");
		}
		printf("├── %d\n", ((int *)h.values)[h.refs[t]]);
		b[bSize] = 1;
		printHeapHelper(h, t, b, bSize+1);
	}
	t = heapLeft(i);
	if(t < h.heapSize) {
		for(int j = 0; j < bSize; ++j) {
			b[j] ? printf("│   ") : printf("    ");
		}
		printf("└── %d\n", ((int *)h.values)[h.refs[t]]);
		b[bSize] = 0;
		printHeapHelper(h, t, b, bSize+1);
	}
}

void intHeapPrint(Heap h) {
	if(h.heapSize > 0) {
		bool *const b = calloc((unsigned)heapHeight(h), sizeof(bool));
		printf("%d\n", ((int *)h.values)[h.refs[0]]);
		printHeapHelper(h, 0, b, 0);
		free(b);
	}
}
