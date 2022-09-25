#define _DEFAULT_SOURCE // NOLINT(bugprone-reserved-identifier)
#include <time.h>

#undef _DEFAULT_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "types.h"

// 1/relative error required
#define RELATIVE_ERROR_INV 1000
// amount of tests for a given input size
#define BENCH_PASSES 100
// whether to regenerate input between passes (will affect measurements for large inputs)
#define BENCH_REGENERATE_DATA true

extern void lputs(char s[const]);
extern int intRandom(void);
extern int compareLongDouble(void const *a, void const *b);
extern void arrayRandom(int *v, int vLength);
extern void printElapsedTime(time_t elapsed);
extern int quickSelect(int v[], int vLength, int k);
extern int heapSelect(int v[], int vLength, int k);
extern int mdmSelect(int v[], int vLength, int k);
extern struct timespec timespecDif(struct timespec a, struct timespec b);
extern struct timespec timespecSum(struct timespec a, struct timespec b);
extern struct timespec timespecMul(struct timespec a, int b);
extern struct timespec timespecDiv(struct timespec a, int b);

typedef struct timespec Timespec;

/**
 * Calculates the actual monotonic clock's resolution by quickly polling to detect its first update. The process is
 * repeated queries times and the results are averaged
 * @param queries number of attempts to detect the lowest visible change
 * @return The lowest detectable interval
 */
Timespec clockResolution(int const queries) {
	Timespec sum = TIMESPEC_ZERO;
	for(int i = 0; i < queries; ++i) {
		Timespec diff, start, end;
		clock_gettime(CLOCK_MONOTONIC, &start);
		do {
			clock_gettime(CLOCK_MONOTONIC, &end);
			diff = timespecDif(end, start);
		} while(diff.tv_sec == 0 && diff.tv_nsec == 0);
		sum = timespecSum(sum, diff);
	}
	return timespecDiv(sum, queries);
}

/**
 * Calculates the average of an array of time intervals
 * @param timings array of time intervals
 * @param timingsLength length of timings
 * @return The averaged time interval
 */
Timespec timespecAvg(Timespec const timings[const], int const timingsLength) {
	assert(timings);
	Timespec sum = TIMESPEC_ZERO;

	for(int i = 0; i < timingsLength; ++i) {
		sum = timespecSum(sum, timings[i]);
	}
	return timespecDiv(sum, timingsLength);
}

/**
 * Calculates the standard deviation of an array of time intervals
 * @param avg the average of the time intervals
 * @param timings array of time intervals
 * @param timingsLength length of timings
 * @return The standard deviation
 */
long double timespecSd(Timespec const avg, Timespec const timings[const], int const timingsLength) {
	assert(timings);
	long double *const squares = malloc((unsigned)timingsLength*sizeof(long double));

	for(int i = 0; i < timingsLength; ++i) {
		Timespec const dif = timespecDif(timings[i], avg);
		squares[i] = dif.tv_sec+dif.tv_nsec/1000000000.L;
		squares[i] *= squares[i];
	}

	// ordered floating point sum minimizes precision losses
	qsort(squares, (unsigned)timingsLength, sizeof(long double), compareLongDouble);

	long double sum = 0;
	for(int i = 0; i < timingsLength; ++i) {
		sum += squares[i];
	}
	free(squares);
	return sqrtl(sum/timingsLength);
}

/**
 * Measures the computation times of the specified selection algorithm on randomized data of provided size. The test is
 * repeated on the same data till the total time is >= threshold, then the result is averaged.
 * The entire process is repeated BENCH_PASSES times, then the average and standard deviation of the resulting timings
 * are printed.
 * @param v pointer to a memory location where the data will be generated
 * @param vLength length of vLength
 * @param k index in sorted v
 * @param threshold minimum time interval
 * @param select a selection function
 */
void bench(int *const v, int const vLength, int const k, Timespec const threshold, int (*const select)(int[], int, int)) {
	assert(v);
	assert(select);
	Timespec timings[BENCH_PASSES];
	int *const vTemp = BENCH_REGENERATE_DATA ? malloc((unsigned)vLength*sizeof(int)) : NULL;

	if(BENCH_REGENERATE_DATA && vTemp == NULL) {
		fprintf(stderr, "not enough memory to regenerate input (requires %lu bytes)\n", (unsigned)vLength*sizeof(int));
	}

	for(int i = 0; i < BENCH_PASSES; ++i) {
		Timespec diff, start, end;
		int queries = 0;

		arrayRandom(v, vLength);
		if(vTemp) {
			// backup array in vTemp
			memcpy(vTemp, v, (unsigned)vLength*sizeof(int));
		}
		clock_gettime(CLOCK_MONOTONIC, &start);
		do {
			select(v, vLength, k);
			if(vTemp) {
				// regenerate array
				memcpy(v, vTemp, (unsigned)vLength*sizeof(int));
			}
			clock_gettime(CLOCK_MONOTONIC, &end);
			diff = timespecDif(end, start);
			++queries;
			// repeat until time measured is >= threshold
		} while(diff.tv_sec < threshold.tv_sec || (diff.tv_sec == threshold.tv_sec && diff.tv_nsec < threshold.tv_nsec));
		timings[i] = timespecDiv(diff, queries);
	}
	free(vTemp);
	Timespec const avg = timespecAvg(timings, BENCH_PASSES);
	long double const sd = timespecSd(avg, timings, BENCH_PASSES);
	printf("avg=%ld.%09lus sd=%Lg\n", avg.tv_sec, avg.tv_nsec, sd);
}

int main(void) {
	Timespec programStart, programEnd;
	clock_gettime(CLOCK_MONOTONIC, &programStart);
	srand((unsigned)time(NULL));

	// threshold is the minimum time measurement necessary to have a relative error of 1/RELATIVE_ERROR_INV
	// threshold = resolution*(1/(relative error) + 1)
	Timespec const threshold = timespecMul(clockResolution(100), RELATIVE_ERROR_INV+1);

	printf("threshold: %ld.%09lus\n", threshold.tv_sec, threshold.tv_nsec);
	printf("passes: %d\n", BENCH_PASSES);
	printf("input regeneration %s\n\n", BENCH_REGENERATE_DATA ? "enabled" : "disabled");

	// y = a*2^(x*b) where {x=0 y=100}, {x=99 y=5000000}
	double const a = 100;
	double const b = log2(50000)/99;

	for(int j = 0; j < 4; ++j) {
		for(int i = 0; i < 100; ++i) {
			// input size follows an exponential curve from 100 to 5000000
			int const vLength = (int)ceil(a*exp2(i*b));
			int k;

			switch(j) {
				case 0:
					k = 70;
					break;
				case 1:
					k = (int)log(vLength);
					break;
				case 2:
					k = vLength/4;
					break;
				case 3:
					k = vLength/3*2;
					break;
				default:
					assert(false);
			}
			printf("vLength=%d k=%d\n", vLength, k);

			// main input memory allocation is done once per input size. If BENCH_REGENERATE_DATA, each benchmark will allocate
			// an equally sized memory chunk to regenerate the input data.
			int *const v = malloc((unsigned)vLength*sizeof(int));
			if(v == 0) {
				fputs("out of memory", stderr);
				return 1;
			}

			lputs("quickSelect: ");
			bench(v, vLength, k, threshold, quickSelect);
			lputs("heapSelect: ");
			bench(v, vLength, k, threshold, heapSelect);
			lputs("mdmSelect: ");
			bench(v, vLength, k, threshold, mdmSelect);

			free(v);
			puts("");

			#ifdef DEBUG
			if(vLength > 300) {
				break;
			}
			#endif
		}
	}
	clock_gettime(CLOCK_MONOTONIC, &programEnd);
	lputs("bench took ");
	printElapsedTime(timespecDif(programEnd, programStart).tv_sec);
	puts("\ndone");
	return 0;
}
