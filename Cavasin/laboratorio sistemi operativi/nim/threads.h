#ifndef NIM__THREADS_H_
#define NIM__THREADS_H_

#include <pthread.h>
#include <values.h>

/*
 * 0: undertaker
 */
#define RESERVED_THRIDS 1

#define joinThread(THREAD) pthread_join((THREAD), NULL)
#define lock(...) pthread_mutex_lock(__VA_ARGS__)
#define unlock(...) pthread_mutex_unlock(__VA_ARGS__)
#define wait(...) pthread_cond_wait(__VA_ARGS__)
#define signal(...) pthread_cond_signal(__VA_ARGS__)

#define unpackThrArgs(TYPE, STRUCT) const unsigned int thrId=thrArgs->thrId; \
                                    TYPE *const (STRUCT)=(TYPE *const)thrArgs->thrFunctionArgs;  \
                                    free(thrArgs)

#define threadSelf() pthread_self()

unsigned int currentThrId = UINT_MAX-1;

typedef struct {
	unsigned int thrId;
	void *thrFunctionArgs;
} ThrArgs;

void createdReservedThread(pthread_t *const thr, const unsigned int thrId, void *(*const thrFunction)(void *), void *const thrFunctionArgs) {
	pthread_t currentThr;
	ThrArgs *const thrArgs = malloc(sizeof(ThrArgs));
	*thrArgs = (ThrArgs){.thrId=thrId, .thrFunctionArgs=thrFunctionArgs};

	if(pthread_create(&currentThr, NULL, thrFunction, thrArgs) != 0) {
		perror("createThread");
		exit(1);
	}
	if(thr) {
		*thr = currentThr;
	}
}

void createThread(pthread_t *const thr, unsigned int *const thrId, void *(*const thrFunction)(void *), void *const thrFunctionArgs) {
	currentThrId = (currentThrId-RESERVED_THRIDS+1)%(UINT_MAX-RESERVED_THRIDS)+RESERVED_THRIDS;
	pthread_t currentThr;
	ThrArgs *const thrArgs = malloc(sizeof(ThrArgs));
	*thrArgs = (ThrArgs){.thrId=currentThrId, .thrFunctionArgs=thrFunctionArgs};

	if(pthread_create(&currentThr, NULL, thrFunction, thrArgs) != 0) {
		perror("createThread");
		exit(1);
	}
	if(thr) {
		*thr = currentThr;
	}
	if(thrId) {
		*thrId = currentThrId;
	}
}

#endif //NIM__THREADS_H_
