#include <stdio.h>
#include <values.h>

#define RESERVED_THRIDS 0
unsigned int currentThrId = UINT_MAX-1;

void test1() {
	for(unsigned long i = 1; i <= UINT_MAX+1ul; i++) {
		currentThrId = (currentThrId-RESERVED_THRIDS+1)%(UINT_MAX-RESERVED_THRIDS)+RESERVED_THRIDS;
		if(i == 1 || i == 2 || i == UINT_MAX || i == UINT_MAX+1ul) {
			printf("%u\n", currentThrId);
		}
	}
}

int main() {


	test1();

	return 0;
}