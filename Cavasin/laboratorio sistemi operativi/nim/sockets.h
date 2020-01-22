#ifndef NIM__SOCKETS_H_
#define NIM__SOCKETS_H_

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stddef.h>
#include <stdlib.h>

#define printSckAttr(SCKATTR) printf("SckAttr:\n" \
                                     "sun_family=%d" \
                                     "\tsun_path=%s" \
                                     "\n", \
                                     (SCKATTR).sun_family, (SCKATTR).sun_path)

#define sendStruct(SCK, STRUCT) sendData((SCK),(STRUCT),sizeof(*(STRUCT)))
#define receiveStruct(SCK, STRUCT) receiveData((SCK),(STRUCT),sizeof(*(STRUCT)))
;

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
	*sck = 0;
}

void sendData(const int sck, const void *const data, const size_t dataSize) {
	if(send(sck, data, dataSize, 0) < 0) {
		perror("sendData");
	}
}

void receiveData(const int sck, void *const data, const size_t dataSize) {
	if(recv(sck, data, dataSize, 0) < 0) {
		perror("receiveData");
	}
}

// SERVER
void initializeSocketAttr(SckAttr *const serverSckAttr, const char sunPath[const]) {
	serverSckAttr->sun_family = AF_UNIX;
	strcpy(serverSckAttr->sun_path, sunPath);
}

void bindSocket(const int serverSck, const SckAttr *const serverSckAttr) {
	remove(serverSckAttr->sun_path);
	if(bind(serverSck, (const struct sockaddr *const)serverSckAttr, strlen(serverSckAttr->sun_path)*sizeof(char)+sizeof(serverSckAttr->sun_family)) == -1) {
		perror("bindSocket");
		exit(1);
	}
}

void listenSocket(const int serverSck, const unsigned short maxPendingRequests) {
	if(listen(serverSck, maxPendingRequests) == -1) {
		perror("listenSocket");
		exit(1);
	}
}

void acceptConnection(const int serverSck, int *const clientSck, SckAttr *const clientSckAttr) {
	SckAttrSize sckAttrSize;
	*clientSck = accept(serverSck, (struct sockaddr *const)clientSckAttr, &sckAttrSize);
	if(*clientSck == -1) {
		perror("acceptConnection");
		exit(1);
	}
}

// CLIENT
void connectSocket(const int clientSck, const SckAttr *const clientSckAttr) {
	const SckAttrSize sckAttrSize = offsetof(SckAttr, sun_path)+strlen(clientSckAttr->sun_path)+sizeof(char); //an abstract socket address is distinguished from a pathname socket by the fact that sun_path[0] is '\0'
	if(connect(clientSck, (const struct sockaddr *const)clientSckAttr, sckAttrSize) == -1) {
		perror("connectSocket");
		exit(1);
	}
}

#endif //NIM__SOCKETS_H_
