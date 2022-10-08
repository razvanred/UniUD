#ifndef ASD_LAB_TYPES_H
#define ASD_LAB_TYPES_H

#include <time.h>

#define TIMESPEC_ZERO (struct timespec) {.tv_sec=0, .tv_nsec=0}

/**
 * A comparison function that takes two indices to some data passed as argument, and outputs -1 if data at first index
 * < data at second index, 0 if data at first index = data at second index, 1 if data at first index > data at second
 * index
 */
typedef int (*Comparator)(void const *, int, int);

/**
 * A struct of a generic, index-based heap. Index-based means that the heap provides and receives indices to some
 * external data, not the values directly. This also makes it generic, meaning it can operate with data of unknown type.
 */
typedef struct Heap {
	int *const refs; /*!< an array of indices to data in values. References will be compared to eachother using the
 									 provided comparison function */
	int const refsLength;  //!< length of refs >= maxSize
	int const maxSize; //!< max heap size, must be <= refsLength
	int heapSize; //!< current length of the heap
	Comparator const compare; //!< the comparison function for this particular heap
	void const *const values; /*!< a pointer to any data that will be available to the comparison function. This
 														separation makes possible to work with heaps of unknown data. */
} Heap;

#endif //ASD_LAB_TYPES_H
