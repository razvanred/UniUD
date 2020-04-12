#include "vector.h"

int parent(int root) {
	return (root - 1) >> 1;
}

int right(int root) {
	return (root + 1) << 1;
}

int left(int root) {
	return (root << 1) + 1;
}
