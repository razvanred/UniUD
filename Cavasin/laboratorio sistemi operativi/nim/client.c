#include <stdio.h>
#include "sockets.h"
#include "common.h"

#define SOCK_PATH "echo_socket"

typedef struct {
	Player playerA, playerB;
	int clientSck;
} ClientMatch;

void connectToServer(ClientMatch *const clientMatch) {
	SckAttr clientSckAttr;
	initializeSocket(&clientMatch->clientSck);
	initializeSocketAttr(&clientSckAttr, SOCK_PATH);

	puts("Trying to connect...");
	connectSocket(clientMatch->clientSck, &clientSckAttr);
	puts("Connected");
}

void initializeMatch(ClientMatch *const clientMatch) {
	sendStruct(clientMatch->clientSck, &clientMatch->playerA);
	puts("sent");
	receiveStruct(clientMatch->clientSck, &clientMatch->playerB);
	clientMatch->playerA.id = !clientMatch->playerB.id;
	printf("Match started with %s\n", clientMatch->playerB.name);
}

void playMatch(const ClientMatch *const clientMatch) {
	MatchStatus matchStatus;

	do {
		receiveStruct(clientMatch->clientSck, &matchStatus);
		//update gui
		if(matchStatus.turn == clientMatch->playerA.id) {
			unsigned short tower, amount;
			printf("Tower (1/0): ");
			scanf("%hu", &tower); // NOLINT(cert-err34-c)
			printf("Amount: ");
			scanf("%hu", &amount); // NOLINT(cert-err34-c)
			sendStruct(clientMatch->clientSck, &((Move){.tower=tower, .amount=amount}));
		}
	} while(1);
}

int main(int argc, const char *argv[]) {
	ClientMatch clientMatch;

	printf("enter name: ");
	scanf("%20s", clientMatch.playerA.name);

	connectToServer(&clientMatch);
	initializeMatch(&clientMatch);
	playMatch(&clientMatch);

	closeSocket(&clientMatch.clientSck);

	return 0;
}
