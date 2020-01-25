#include <stdio.h>
#include "sockets.h"
#include "threads.h"
#include "common.h"

#define sendStructBoth(SERVERMATCH, STRUCT) sendStruct((SERVERMATCH)->serverPlayerA.sck, (STRUCT)) + sendStruct((SERVERMATCH)->serverPlayerB.sck, (STRUCT))

typedef struct {
	Player player;
	int sck;
} ServerPlayer;

typedef struct {
	pthread_mutex_t *undertakerLock, *matchesLock;
	pthread_cond_t *undertakerCond;
	struct ServerMatch **undertakerBuffer;
} Synchronized;

typedef struct ServerMatch {
	pthread_t thr;
	unsigned int thrId;
	Synchronized synchronized;
	ServerPlayer serverPlayerA, serverPlayerB;
} ServerMatch;

void *matchRoutine(ThrArgs *const thrArgs) {
	unpackThrArgs(ServerMatch, allocatedServerMatch);
	printf("spawned match %d\n", thrId);

	allocatedServerMatch->thrId = thrId;
	allocatedServerMatch->thr = threadSelf();
	allocatedServerMatch->serverPlayerA.player.id = true;
	allocatedServerMatch->serverPlayerB.player.id = false;
	sendStruct(allocatedServerMatch->serverPlayerA.sck, &allocatedServerMatch->serverPlayerB.player);
	sendStruct(allocatedServerMatch->serverPlayerB.sck, &allocatedServerMatch->serverPlayerA.player);
	printf("match %d started between \"%s\" and \"%s\"\n", thrId, allocatedServerMatch->serverPlayerA.player.name, allocatedServerMatch->serverPlayerB.player.name);

	MatchStatus matchStatus = {
			.tower1Height=getRand(4, TOWERSMAXHEIGHT-2),
			.tower2Height=getRand(4, TOWERSMAXHEIGHT-2),
			.turn=getRand(0, 1)
	};
	Move move;

	do {
		breakIfFalse(sendStructBoth(allocatedServerMatch, &matchStatus))
		if(matchStatus.tower1Height == TOWERSMAXHEIGHT && matchStatus.tower2Height == TOWERSMAXHEIGHT) {
			break;
		}
		if(matchStatus.turn) {
			breakIfFalse(receiveStruct(allocatedServerMatch->serverPlayerA.sck, &move))
		} else {
			breakIfFalse(receiveStruct(allocatedServerMatch->serverPlayerB.sck, &move))
		}
		if(move.amount+(move.tower ? matchStatus.tower1Height : matchStatus.tower2Height) <= TOWERSMAXHEIGHT && move.amount > 0) {
			if(move.tower) {
				matchStatus.tower1Height += move.amount;
			} else {
				matchStatus.tower2Height += move.amount;
			}
			matchStatus.turn = !matchStatus.turn;
		}
	} while(1);
	printf("match %d ended\n", thrId);
	lock(allocatedServerMatch->synchronized.matchesLock);
	lock(allocatedServerMatch->synchronized.undertakerLock);

	*(allocatedServerMatch->synchronized.undertakerBuffer) = allocatedServerMatch;

	signal(allocatedServerMatch->synchronized.undertakerCond);
	unlock(allocatedServerMatch->synchronized.undertakerLock);
	return NULL;
}

void startMatch(ServerMatch *const allocatedServerMatch) {
	createThread(NULL, NULL, (void *(*)(void *))&matchRoutine, allocatedServerMatch);
}

ServerMatch *allocateMatch(const Synchronized synchronized, const ServerPlayer serverPlayerA, const ServerPlayer serverPlayerB) {
	ServerMatch *const serverMatch = malloc(sizeof(ServerMatch));
	*serverMatch = (ServerMatch){.synchronized=synchronized, .serverPlayerA = serverPlayerA, .serverPlayerB = serverPlayerB};
	return serverMatch;
}

void matchMaker(const int bindedSck, const Synchronized synchronized) {
	ServerPlayer serverPlayerA, serverPlayerB;

	puts("served started");
	while(1) {
		acceptConnection(bindedSck, &serverPlayerA.sck);
		receiveStruct(serverPlayerA.sck, &serverPlayerA.player);
		printf("matchMaker: connected to player A \"%s\"\n", serverPlayerA.player.name);

		acceptConnection(bindedSck, &serverPlayerB.sck);
		receiveStruct(serverPlayerB.sck, &serverPlayerB.player);
		printf("matchMaker: connected to player B \"%s\"\n", serverPlayerB.player.name);

		startMatch(allocateMatch(synchronized, serverPlayerA, serverPlayerB));
	}
}

void *undertaker(ThrArgs *const thrArgs) {
	unpackThrArgs(Synchronized, synchronized);
	printf("undertaker ID %d\n", thrId);
	ServerMatch *allocatedServerMatch;
	unsigned int deadThrId;

	while(1) {
		lock(synchronized->undertakerLock);
		while(!*(synchronized->undertakerBuffer)) {
			wait(synchronized->undertakerCond, synchronized->undertakerLock);
		}

		allocatedServerMatch = *synchronized->undertakerBuffer;
		deadThrId = allocatedServerMatch->thrId;
		joinThread(allocatedServerMatch->thr);
		closeSocket(&allocatedServerMatch->serverPlayerA.sck);
		closeSocket(&allocatedServerMatch->serverPlayerB.sck);
		free(allocatedServerMatch);
		*synchronized->undertakerBuffer = NULL;

		unlock(synchronized->undertakerLock);
		unlock(synchronized->matchesLock);

		printf("undertaker: deleted thread %d\n", deadThrId);
	}
}

int main(/*int argc, const char *argv[]*/) {
	int bindedSck;
	SckAttr bindedSckAttr;
	pthread_mutex_t undertakerLock = PTHREAD_MUTEX_INITIALIZER, matchesLock = PTHREAD_MUTEX_INITIALIZER;
	pthread_cond_t undertakerCond = PTHREAD_COND_INITIALIZER;
	ServerMatch *undertakerBuffer = NULL;
	Synchronized synchronized = {
			.undertakerLock=&undertakerLock,
			.matchesLock = &matchesLock,
			.undertakerCond = &undertakerCond,
			.undertakerBuffer=&undertakerBuffer
	};
	initializeRand();

	initializeSocket(&bindedSck);
	initializeSocketAttr(&bindedSckAttr, SCK_PATH);
	bindSocket(bindedSck, &bindedSckAttr);
	printSckAttr(bindedSckAttr);

	listenSocket(bindedSck, 100);

	createdReservedThread(NULL, 0, (void *(*)(void *))&undertaker, &synchronized);
	matchMaker(bindedSck, synchronized);
	return 0;
}
