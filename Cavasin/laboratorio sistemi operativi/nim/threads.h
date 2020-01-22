#ifndef NIM__THREADS_H_
#define NIM__THREADS_H_

#include <pthread.h>
#include <values.h>

/*
 *
 */
#define RESERVED_THRIDS 0

#define lock(...) pthread_mutex_lock(__VA_ARGS__)
#define unlock(...) pthread_mutex_unlock(__VA_ARGS__)
#define wait(...) pthread_cond_wait(__VA_ARGS__)
#define signal(...) pthread_cond_signal(__VA_ARGS__)

unsigned int currentThrId = UINT_MAX-1;

typedef struct {
	const unsigned int thrId;
	void *const thrFunctionArgs;
} thrArgs;

void createThread(pthread_t *const thr, unsigned int *const thrId, void *(*const thrFunction)(void *), void *const thrFunctionArgs) {
	currentThrId = (currentThrId-RESERVED_THRIDS+1)%(UINT_MAX-RESERVED_THRIDS)+RESERVED_THRIDS;
	pthread_t currentThr;

	if(pthread_create(&currentThr, NULL, thrFunction, &(thrArgs){.thrId=currentThrId, .thrFunctionArgs=thrFunctionArgs}) != 0) {
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
