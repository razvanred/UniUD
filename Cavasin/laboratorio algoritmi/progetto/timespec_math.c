#include <time.h>
#include <assert.h>

struct timespec timespecDif(struct timespec const a, struct timespec const b) {
	struct timespec r = {
			r.tv_sec = a.tv_sec-b.tv_sec, r.tv_nsec = a.tv_nsec-b.tv_nsec
	};
	if(r.tv_nsec < 0) {
		--(r.tv_sec);
		r.tv_nsec += 1000000000L;
	}
	return r;
}

struct timespec timespecSum(struct timespec const a, struct timespec const b) {
	struct timespec r = {
			.tv_sec = a.tv_sec+b.tv_sec, .tv_nsec = a.tv_nsec+b.tv_nsec
	};
	if(r.tv_nsec > 1000000000L) {
		++(r.tv_sec);
		r.tv_nsec -= 1000000000L;
	}
	return r;
}

//(a+b/1E9)*c = c*a+cb/1E9
struct timespec timespecMul(struct timespec const a, long const b) {
	assert(b >= 0);

	// usual arithmetic conversion to long long for greater precision
	time_t t = (time_t)b*a.tv_nsec;
	return (struct timespec){
			.tv_sec = ((time_t)b*a.tv_sec)+(t/1000000000LL), .tv_nsec = (long)(t%1000000000LL)
	};
}

//(a+b/1E9)/c = a/c+b/cE9
struct timespec timespecDiv(struct timespec const a, long const b) {
	assert(b >= 0);

	// usual arithmetic conversion to long long for greater precision
	time_t t = (((a.tv_sec%(time_t)b)*1000000000LL)+a.tv_nsec)/b;
	return (struct timespec){
			.tv_sec = (a.tv_sec/(time_t)b)+(t/1000000000LL), .tv_nsec = (long)(t%1000000000LL)
	};
}
