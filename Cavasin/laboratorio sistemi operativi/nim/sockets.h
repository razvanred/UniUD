#ifndef NIM__SOCKETS_H_
#define NIM__SOCKETS_H_

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stddef.h>
#include <stdlib.h>
#include <errno.h>
#include <stdbool.h>

#define printSckAttr(SCKATTR) printf("SckAttr:\n" \
                                     "sun_family=%d" \
                                     "\tsun_path=%s" \
                                     "\n", \
                                     (SCKATTR).sun_family, (SCKATTR).sun_path)

#define sizeofSckAttr(SCKATTR) (offsetof(SckAttr, sun_path)+(strlen((SCKATTR)->sun_path)+1)*sizeof(char)) //an abstract socket address is distinguished from a pathname socket by the fact that sun_path[0] is '\0'

#define sendStruct(SCK, STRUCT) sendData((SCK),(STRUCT),sizeof(*(STRUCT)))
#define receiveStruct(SCK, STRUCT) receiveData((SCK),(STRUCT),sizeof(*(STRUCT)))

typedef struct sockaddr_un SckAttr;
typedef socklen_t SckAttrSize;

void initializeSocket(int *const sck) {
	*sck = socket(AF_UNIX, SOCK_STREAM, 0);
	if(*sck == -1) {
		perror("initializeSocket");
		exit(1);
	}
}

void closeSocket(int *const sck) {
	close(*sck);
	*sck = -1;
}

bool sendData(const int sck, const void *const data, const size_t dataSize) {
	if(send(sck, data, dataSize, MSG_NOSIGNAL) <= 0) {
		if(errno != EPIPE) {
			perror("receiveData");
		}
		return false;
	} else {
		return true;
	}
}

bool receiveData(const int sck, void *const data, const size_t dataSize) {
	const int recvRet = recv(sck, data, dataSize, 0);
	if(recvRet < (int)dataSize) {
		if(recvRet < 0) {
			perror("sendData");
		}
		return false;
	} else {
		return true;
	}
}

// SERVER
void initializeSocketAttr(SckAttr *const bindedSckAttr, const char sunPath[const]) {
	bindedSckAttr->sun_family = AF_UNIX;
	strcpy(bindedSckAttr->sun_path, sunPath);
}

void bindSocket(const int bindedSck, const SckAttr *const bindedSckAttr) {
	remove(bindedSckAttr->sun_path);
	if(bind(bindedSck, (const struct sockaddr *const)bindedSckAttr, sizeofSckAttr(bindedSckAttr)) == -1) {
		perror("bindSocket");
		exit(1);
	}
}

void listenSocket(const int bindedSck, const unsigned short maxPendingRequests) {
	if(listen(bindedSck, maxPendingRequests) == -1) {
		perror("listenSocket");
		exit(1);
	}
}

void acceptConnection(const int bindedSck, int *const clientSck) {
	do {
		*clientSck = accept(bindedSck, NULL, NULL);
		if(*clientSck == -1 && errno != ECONNABORTED) {
			perror("acceptConnection");
			exit(1);
		}
	} while(*clientSck == -1);
}

// CLIENT
bool connectSocket(const int clientSck, const SckAttr *const bindedSckAttr) {
	if(connect(clientSck, (const struct sockaddr *const)bindedSckAttr, sizeofSckAttr(bindedSckAttr)) == -1) {
		if(errno != ECONNREFUSED && errno != ENOENT) {
			perror("connectSocket");
			exit(1);
		}
		return false;
	} else {
		return true;
	}
}

#endif //NIM__SOCKETS_H_
