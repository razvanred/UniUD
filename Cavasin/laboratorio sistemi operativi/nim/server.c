#include <stdio.h>
#include "sockets.h"
#include "threads.h"
#include "common.h"

#define SOCK_PATH "echo_socket"

#define sendStructBoth(SERVERMATCH, STRUCT) sendStruct((SERVERMATCH)->serverPlayerA.sck, (STRUCT)); \
                                            sendStruct((SERVERMATCH)->serverPlayerB.sck, (STRUCT))
;

typedef struct {
	Player player;
	int sck;
} ServerPlayer;

typedef struct {
	pthread_t thr;
	pthread_mutex_t *undertakerLock, *matchesLock;
	pthread_cond_t *undertakerCond;
	ServerPlayer serverPlayerA, serverPlayerB;
} ServerMatch;

ServerMatch initializeMatch(pthread_mutex_t *const undertakerLock, pthread_mutex_t *const matchesLock, pthread_cond_t *const undertakerCond, const ServerPlayer playerA, const ServerPlayer playerB) {
	return (ServerMatch){.undertakerLock = undertakerLock, .matchesLock = matchesLock, .undertakerCond = undertakerCond, .serverPlayerA = playerA, .serverPlayerB = playerB,};
}

void *matchRoutine(ServerMatch *serverMatch) {
	serverMatch->serverPlayerA.player.id = true;
	serverMatch->serverPlayerB.player.id = false;
	sendStruct(serverMatch->serverPlayerA.sck, &serverMatch->serverPlayerB.player);
	sendStruct(serverMatch->serverPlayerB.sck, &serverMatch->serverPlayerA.player);

	MatchStatus matchStatus = {
			.tower1Height=getRand(5, 15),
			.tower2Height=getRand(5, 15),
			.turn=getRand(0, 1)
	};
	Move move;

	do {
		sendStructBoth(serverMatch, &matchStatus);
		if(matchStatus.turn) {
			receiveStruct(serverMatch->serverPlayerA.sck, &move);
		} else {
			receiveStruct(serverMatch->serverPlayerB.sck, &move);
		}
		if(move.amount <= (move.tower ? matchStatus.tower1Height : matchStatus.tower2Height)) {
			if(move.tower) {
				matchStatus.tower1Height -= move.amount;
			} else {
				matchStatus.tower2Height -= move.amount;
			}
		}
	} while(1);
}

void startMatch(ServerMatch serverMatch) {
	createThread(NULL, NULL, (void *(*)(void *))&matchRoutine, &serverMatch);
}

void undertaker() {
}

void matchMaker(const int serverSck, pthread_mutex_t *const undertakerLock, pthread_mutex_t *const matchesLock, pthread_cond_t *const undertakerCond) {
	SckAttr clientSckAttr;
	ServerPlayer playerA, playerB;

	while(1) {
		puts("Waiting for player A...");
		acceptConnection(serverSck, &playerA.sck, &clientSckAttr);
		receiveStruct(playerA.sck, &playerA.player);
		printf("Connected to \"%s\"\n", playerA.player.name);
		printSckAttr(clientSckAttr);
		putchar('\n');

		puts("Waiting for player B...");
		acceptConnection(serverSck, &playerB.sck, &clientSckAttr);
		receiveStruct(playerB.sck, &playerB.player);
		printf("Connected to \"%s\"\n", playerB.player.name);
		printSckAttr(clientSckAttr);
		putchar('\n');

		startMatch(initializeMatch(undertakerLock, matchesLock, undertakerCond, playerA, playerB));
	}
}

int main(int argc, const char *argv[]) {
	int serverSck;
	SckAttr serverSckAttr;
	pthread_mutex_t undertakerLock = PTHREAD_MUTEX_INITIALIZER, matchesLock = PTHREAD_MUTEX_INITIALIZER;
	pthread_cond_t undertakerCond = PTHREAD_COND_INITIALIZER;

	initializeSocket(&serverSck);
	initializeSocketAttr(&serverSckAttr, SOCK_PATH);
	bindSocket(serverSck, &serverSckAttr);
	printSckAttr(serverSckAttr);

	listenSocket(serverSck, 100);

	matchMaker(serverSck, &undertakerLock, &matchesLock, &undertakerCond);

	return 0;
}
