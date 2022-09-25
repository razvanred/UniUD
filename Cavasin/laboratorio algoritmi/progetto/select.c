#include <assert.h>
#include <stdbool.h>
#include "types.h"

#define INPUT_SIZE 10000

extern int arraySwap(int v[const], int i, int k);
extern int min(int a, int b);
extern Heap heapAlloc(int size, int maxSize, Comparator compare, void const *values);
extern void heapFree(Heap h);
extern int heapPop(Heap *h);
extern int heapPush(Heap *h, int i);
extern int heapLeft(int i);
extern int heapRight(int i);
extern int compareInt(void const *a, void const *b);

/**
 * Splits v in two halves [p..., pivot, ...q] with v[p...pivot-1] <= v[pivot] < v[pivot+1...q]
 * time: Θ(q-p+1)=Θ(n)
 * @param v array of integers
 * @param p lower bound of interval
 * @param q upper bound of interval
 * @param pivot index of the pivot
 * @return new index of pivot
 */
static int partition(int v[const], int p, int const q, int const pivot) {
	assert(p <= pivot && pivot <= q);

	arraySwap(v, pivot, q);
	int left = p;
	for(; p <= q; ++p) {
		if(v[p] <= v[q]) {
			arraySwap(v, left++, p);
		}
	}
	return left-1;
}

// QUICK SELECT

/**
 * Finds the element that would end up at index k if v[p...q] were sorted, based on partition()
 * modifies v
 * tail call optimizable
 * time: O(gauss sum)=O((k-p+1)²)=O(n²)
 * @param v array of integers
 * @param p lower bound of interval
 * @param q upper bound of interval
 * @param k index in sorted v
 * @return The index in v of the k+1º smallest element
 */
static int quickSelectHelper(int v[const], int const p, int const q, int const k) {
	assert(p <= k || k <= q);

	int const pivot = partition(v, p, q, k);
	if(pivot == k) {
		return k;
	}
	if(k < pivot) {
		return quickSelectHelper(v, p, pivot-1, k);
	}
	return quickSelectHelper(v, pivot+1, q, k);
}

/**
 * Wrapper for quickSelectHelper
 * @param v array of integers
 * @param vLength length of v
 * @param k index in sorted v
 * @return The index in v of the k+1º smallest element
 */
int quickSelect(int v[const], int const vLength, int const k) {
	assert(0 <= k || k < vLength);

	return quickSelectHelper(v, 0, vLength-1, k);
}

// HEAP SELECT

/**
 * A basic integer comparison function that operates on integer arrays
 * @param values array of integers
 * @param a index of first element
 * @param b	index of second element
 * @return -1 if values[a] < values[b], 0 if values[a] == values[b], 1 if values[a] > values[b]
 */
static int compareRef(void const *const values, int const a, int const b) {
	int const *const v = values;
	return compareInt(&v[a], &v[b]);
}

/**
 * Function equivalent to compareRef, but with a and b swapped, equivalent to compareRef()*-1
 * @param values array of integers
 * @param a index of first element
 * @param b index of second element
 * @return 1 if values[a] < values[b], 0 if values[a] == values[b], -1 if values[a] > values[b]
 */
static int compareRefInverted(void const *const values, int const a, int const b) {
	int const *const v = values;
	return compareInt(&v[b], &v[a]);
}

/**
 * A comparison function where values (h1) is a heap. Calls h1's compare().
 * @param values (h1) pointer to a heap
 * @param a index of first element, also and index in h1.refs
 * @param b index of second element, also and index in h1.refs
 * @return h1.compare() called on the elements in h1 referenced by a and b
 */
static int compareRefRef(void const *const values, int const a, int const b) {
	Heap const *const h1 = values;
	return h1->compare(h1->values, h1->refs[a], h1->refs[b]);
}

/**
 * Finds the element that would end up at index k if v[i...j] were sorted, by using heaps
 * modifies v
 * time: Θ(n)+O(k)*O(log k)=O(n + k log k)
 * @param v array of integers
 * @param vLength length of v
 * @param k index in sorted v
 * @return The index in v of the k+1º smallest element
 */
int heapSelect(int v[const], int const vLength, int const k) {
	// as a minor optimization, h2 moves from the closest extreme to k. This brings down the maximum number of top level
	// iterations to n/2.
	bool const fromLeft = k <= vLength/2;
	// when iterating from right to left, a maxHeap is needed instead of a minHeap. To achieve this, it's sufficient to
	// optionally invert the comparison operator.
	Heap const h1 = heapAlloc(vLength, vLength, fromLeft ? compareRef : compareRefInverted, v);
	// h2 must also store the corresponding position in h1 of each element. To achieve this, the whole h1 is passed as data.
	// That makes h2 an index-based heap of indices in v.
	Heap h2 = heapAlloc(1, vLength, compareRefRef, &h1);

	for(int i = 0; i < (fromLeft ? k : vLength-k-1); ++i) {
		// ref is an index to a ref in h1
		int const ref = heapPop(&h2);
		if(heapLeft(ref) < h1.heapSize) {
			heapPush(&h2, heapLeft(ref));
			if(heapRight(ref) < h1.heapSize) {
				heapPush(&h2, heapRight(ref));
			}
		}
	}
	int const r = h1.refs[heapPop(&h2)];
	heapFree(h2);
	heapFree(h1);
	return r;
}

// MEDIANS OF MEDIANS SELECT

/**
 * Finds the element that would end up at index k if v[p...q] were sorted, based on selectionSort
 * modifies v
 * time: O(gauss sum)=O((k-p+1)²)=O(n²)
 * @param v array of integers
 * @param p lower bound of interval
 * @param q upper bound of interval
 * @param k index in sorted v
 * @return The index in v of the k+1º smallest element
 */
static int selectionSelect(int v[const], int p, int const q, int const k) {
	assert(p <= k && k <= q);

	//
	for(; p <= k; p++) {
		int min = p;
		for(int j = p+1; j <= q; j++) {
			if(v[j] < v[p]) {
				min = j;
			}
		}
		arraySwap(v, p, min);
	}
	return (p+q)/2;
}

/**
 * Finds the element that would end up at index k if v[p...q] were sorted. Based on partition, but attempts to choose
 * the center-most pivot (i.e. the median).
 * Achieves O(n) time by executing both recursive calls on less than n elements overall
 * modifies v
 * time: O(n)
 * @param v array of integers
 * @param p lower bound of interval
 * @param q upper bound of interval
 * @param k index in sorted v
 * @return The index in v of the k+1º smallest element
 */
static int mdmSelectHelper(int v[const], int const p, int const q, int const k) {
	assert(p <= k && k <= q);

	if(p == q) {
		return p;
	}
	// medians are placed in v from index p to i included
	int i = 0;
	for(; p+i*5 <= q; ++i) {
		// the last block might be smaller than 5 elements
		int const pp = p+i*5, qq = min(pp+4, q);
		// any (preferably lightweight) algorithm can be used, as input is effectively constant (5)
		// lightweight in terms of "hidden" execution costs (allocations, heavy recursion, ecc)
		arraySwap(v, p+i, selectionSelect(v, pp, qq, (qq+pp)/2));
	}
	// condition failed, so i is now one too many the last median's index
	i = p+i-1;
	// i is now the index of the last median
	int const pivot = partition(v, p, q, mdmSelectHelper(v, p, i, (p+i)/2));
	if(k == pivot) {
		return k;
	}
	if(k < pivot) {
		return mdmSelectHelper(v, p, pivot-1, k);
	}
	return mdmSelectHelper(v, pivot+1, q, k);
}

/**
 * Wrapper for mdmSelectHelper
 * @param v array of integers
 * @param vLength length of v
 * @param k index in sorted v
 * @return The index in v of the k+1º smallest element
 */
int mdmSelect(int v[const], int const vLength, int const k) {
	return mdmSelectHelper(v, 0, vLength-1, k);
}
