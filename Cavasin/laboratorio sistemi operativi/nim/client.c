#include <stdio.h>
#include <time.h>
#include "visualt/visualt.h"
#include "sockets.h"
#include "common.h"

typedef struct {
	Player playerA, playerB;
	int clientSck;
} ClientMatch;

// -GRAPHIC-

#define TOWERSOFFSET -9

typedef struct {
	Obj *canvas, *title, *splashTowers, *splashCityline, *prompt, *log, *tCanvas, *towerSection;
} Objs;
Objs objs;

void objLoader() {
	initializeBlankObj(objs.canvas, 1, LTSIZES{{77, 21}});
	initializeStringObj(objs.splashTowers, 1, LTSTRS{"\v\v\v\v\v\v\v\v\v│\n\v\v\v\v\v\v╓──┴──┐\v\v╓──┼─┐\n\v\v\v\v\v\v║▓▓│▒▒│\v\v║▓▓│▒▒╕\n\v\v\v\v\v\v║▓▓│▒▒│\v\v║▓▓│▒▒│\n\v\v\v\v\v\v║▓▓│▒▒│\v\v║▓▓│▒▒│\n\v\v\v\v\v\v║▓▓│▒││\v\v║▓▓│▒▒│\n\v\v\v\v\v\v║▓▓│▒││\v\v║▓▓│▒▒│\n____\v\v║▓▓│▒▓│\v╓▓▓▓│__│\n▒▒│▒__▓▓▓││▓│╤║▓▓▓▒▒│▒│\n▓▓│▒▒│▓█▓▒│▓││║▓▓▓▓▒│▒│\n▓▓│▒▒│▓██│▒│││║▓██▓▒▓▒│\n▓▓│▒▒│██▓│▒│││║▓██▓_▓__\n▓▓││_▓▓█▓│▒││██▓▓██████"});
	initializeStringObj(objs.splashCityline, 1, LTSTRS{"\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v│\n\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┌┴┐\n\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v│▒│\v\v\v\v\v\v\v\v\v\v\v\v┌────┐\v\v\v\v\v┌─┐\v\v\v\v┌┐\v\v\v\v\v\v\v\v\v\v\v┌───┐\n\v\v\v\v\v\v\v\v\v\v\v\v\v\v┌───┐\v\v\v\v│▒│\v\v\v\v\v\v\v┌─┐\v\v│▒░░░│\v\v┌──┘░│\v\v\v┌┘└┐\v\v\v\v\v\v┌───┤▒░░│\n\v\v\v\v\v┌─┐\v\v\v\v┌─┘▒░░└─┐\v┌┘▒│\v\v\v\v\v┌─┘░│\v\v│▒░░░│┌─┘░░░░│\v\v\v│▒▒│\v\v\v\v\v┌┘░░░│▒░░│\n\v\v\v\v┌┘░│\v┌──┘▒▒▒░░░░├─┘▒▒└───┐\v│▒▒░│\v\v│▒░░░││▒▒░░░░│\v\v\v│▒▒│\v\v\v\v┌┘▒░░░│▒░░│\n\v\v\v\v│▒░│\v│▒▒▒▒▒▒░░░░│▒▒▒▒░░░░│\v│▒▒░│\v┌┘▒░░░││▒▒░░░░├───┘▒▒└──┐\v│▒▒░░░└┐░░│\n\v┌──┘▒░│\v│▒▒▒▒▒▒░░░░│▒▒▒▒░░░░│\v│▒▒░│\v│▒▒░░░││▒▒░░░░│▒▒▒▒▒▒░░░│\v│▒▒░░░░│░░│\v"});
	initializeStringObj(objs.title, 1, LTSTRS{"██████\v\v\v\v\v\v███╔═╦═╗╔═══╗╥\v\v\v╥╔═══╡╔═══╗╔═══╡\n█████████\v\v\v███║\v║\v║║\v\v\v║║\v\v\v║║\v\v\v\v║\v\v\v║║\n███\v\v\v\v████\v███╨\v║\v╨║\v\v\v║║\v\v\v║║\v\v\v\v║\v\v\v║║\n███\v\v\v\v\v\v██████\v\v║\v\v║\v\v\v║║\v\v\v║║\v\v\v\v║\v\v\v║║\n▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓\v\v║\v\v║\v\v\v║║\v\v\v║║\v\v\v\v╠══╦╝║\n\v\v\v\v\v\v▓▓▓\v\v\v\v\v\v\v\v║\v\v║\v\v\v║║\v╥\v║╠══╡\v║\v\v║\v╚═══╗\n\v\v\v\v\v\v▓▓▓\v\v\v\v\v\v\v\v║\v\v║\v\v\v║║\v║\v║║\v\v\v\v║\v\v║\v\v\v\v\v║\n▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓\v\v║\v\v║\v\v\v║║\v║\v║║\v\v\v\v║\v\v╚╗\v\v\v\v║\n█████\v\v\v\v\v█████\v\v║\v\v║\v\v\v║║\v║\v║║\v\v\v\v║\v\v\v║\v\v\v\v║\n██████\v\v\v██████\v\v║\v\v║\v\v\v║║\v║\v║║\v\v\v\v║\v\v\v║\v\v\v\v║\n███\v███████\v███\v\v║\v\v║\v\v\v║║\v║\v║║\v\v\v\v║\v\v\v║\v\v\v\v║\n███\v\v\v███\v\v\v███\v\v║\v\v╚═══╝╚═╩═╝╚═══╡╨\v\v\v╨╞═══╝"});
	initializeStringObj(objs.prompt, 6, LTSTRS{"Enter\vyour\vname:\n\n•\v\v\v\v\v\v\v\v\v\v\v\v\v\v•\n\n\nAlvise\v\vBruniera\nRiccardo\vCavasin",
																						 "┌───────────────────┤•··├───────────────────┐\n┴\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┴\n\n\n\n\n\n\n\n┬\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┬\n└───────────────────┤•··├───────────────────┘",
																						 "┌───────────────────┤·•·├───────────────────┐\n┴\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┴\n\n\n\n\n\n\n\n┬\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┬\n└───────────────────┤·•·├───────────────────┘",
																						 "┌───────────────────┤··•├───────────────────┐\n┴\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┴\n\n\n\n\n\n\n\n┬\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┬\n└───────────────────┤··•├───────────────────┘",
																						 "┌───────────────────┤·•·├───────────────────┐\n┴\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┴\n\v\v\v\v\v\v\v\v\v\v\v\v\v\v\vopponent's turn\n┬\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┬\n└───────────────────┤·•·├───────────────────┘",
																						 "┌───────────────────┤·•·├───────────────────┐\n┴\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┴\n\v\v\v\v\v\v\v\v\v\v\vdisconnected from server\n┬\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v\v┬\n└───────────────────┤·•·├───────────────────┘"});
	initializeBlankObj(objs.log, 1, LTSIZES{{1, 1}});
	initializeObjObj(objs.tCanvas, objs.canvas);
	initializeStringObj(objs.towerSection, 2, LTSTRS{"\v░░░░░░░░░\n░░░░░░░░░▒\n▓▓▓▓▓▓▓▓▓\v", "\v░░░░░░░░░\n░░░░░░░░░░\n▒▒▒▒▒▒▒▒▒\v"});
}

void objUnloader() {
	releaseObj(objs.canvas);
	releaseObj(objs.splashTowers);
	releaseObj(objs.splashCityline);
	releaseObj(objs.title);
	releaseObj(objs.prompt);
	releaseObj(objs.log);
	releaseObj(objs.tCanvas);
	releaseObj(objs.towerSection);
}

void logWrite(const char *s, const unsigned short offset) {
	setSpriteText(objs.log, VTSTR s);
	stamp(objs.canvas, 1, VTOBJS&objs.log);
	changeY(NULL, objs.log, -offset);
}

void logSection(const unsigned short section) {
	gotoXY(NULL, objs.prompt, xPosition(objs.title), yPosition(objs.title)+1);
	setSprite(objs.prompt, 1+section);
	stamp(objs.canvas, 1, VTOBJS&objs.prompt);
}

#define logString(...) {                              \
                              char string[100];             \
                              sprintf(string, __VA_ARGS__); \
                              logWrite(string, 1);          \
                              }

void splashLayout() {
	gotoXY(NULL, objs.title, 12, 3);
	gotoXY(NULL, objs.splashTowers, -37, -10);
	align(objs.splashTowers, 2);
	gotoXY(NULL, objs.splashCityline, 37, -10);
	align(objs.splashCityline, 3);
	gotoXY(NULL, objs.prompt, -23, 6);
	render(objs.canvas, 4, LTOBJS{objs.title, objs.splashCityline, objs.splashTowers, objs.prompt});
}//{

void printName(const char *s) {
	char string[100];
	sprintf(string, "Enter\vyour\vname:\n\n•%-14.14s•\n\n\nAlvise\v\vBruniera\nRiccardo\vCavasin", s);
	setSpriteText(objs.prompt, VTSTR string);
	render(objs.canvas, 3, LTOBJS{objs.splashCityline, objs.splashTowers, objs.prompt});
}

void playLayout(const ClientMatch *const clientMatch) {
	gotoXY(NULL, objs.title, 0, -4);
	changeY(NULL, objs.splashCityline, -2);
	logString("╡%s Vs %s╞", clientMatch->playerA.name, clientMatch->playerB.name)
	gotoXY(NULL, objs.log, 0, 10);
}//{

void moveLayout(const MatchStatus *const matchStatus) {
	clear(objs.canvas);
	setSprite(objs.towerSection, 0);
	gotoX(NULL, objs.towerSection, -22);
	for(unsigned int i = 0; i < matchStatus->tower1Height; i++) {
		gotoY(NULL, objs.towerSection, TOWERSOFFSET+(int)i);
		stamp(objs.canvas, 1, VTOBJS&objs.towerSection);
	}
	gotoX(NULL, objs.towerSection, 22);
	for(unsigned int i = 0; i < matchStatus->tower2Height; i++) {
		gotoY(NULL, objs.towerSection, TOWERSOFFSET+(int)i);
		stamp(objs.canvas, 1, VTOBJS&objs.towerSection);
	}
	stamp(objs.canvas, 2, LTOBJS{objs.splashCityline, objs.log});
}//{

void printPreview(const MatchStatus *const matchStatus, const Move *const move) {
	nextSprite(objs.towerSection);
	render(objs.tCanvas, 1, VTOBJS&objs.canvas);
	if(move->tower) {
		gotoXY(NULL, objs.towerSection, -22, TOWERSOFFSET+matchStatus->tower1Height);
	} else {
		gotoXY(NULL, objs.towerSection, 22, TOWERSOFFSET+matchStatus->tower2Height);
	}
	for(unsigned int i = 0; i < move->amount; i++) {
		stamp(objs.tCanvas, 1, VTOBJS&objs.towerSection);
		changeY(NULL, objs.towerSection, 1);
	}
}

//}
void opponentLayout() {
	logSection(3);
	gotoXY(NULL, objs.prompt, 0, 6);
	render(objs.canvas, 3, LTOBJS{objs.title, objs.prompt, objs.log});
}

void disconnection() {
	logSection(4);
	gotoXY(NULL, objs.prompt, 0, 6);
	render(objs.canvas, 2, LTOBJS{objs.title, objs.prompt});
}

void endgame(bool won) {
	setPenChar(objs.log, LTCHAR "¯");
	gotoXY(NULL, objs.log, 22, TOWERSOFFSET+TOWERSMAXHEIGHT);
	gotoX(objs.tCanvas, objs.log, -22);
	setSprite(objs.towerSection, 0);
	for(int i = TOWERSOFFSET; i < TOWERSMAXHEIGHT+ TOWERSOFFSET; i++) {
		gotoXY(NULL, objs.towerSection, -22, i);
		stamp(objs.tCanvas, 1, VTOBJS&objs.towerSection);
		gotoXY(NULL, objs.towerSection, 22, i);
		stamp(objs.tCanvas, 1, VTOBJS&objs.towerSection);
	}
	changeY(NULL, objs.log, 1);
	setSpriteText(objs.log, VTSTR "♀");

	for(unsigned int i = 0; i < 4; i++) {
		render(objs.canvas, 3, LTOBJS{objs.tCanvas, objs.log, objs.splashCityline});
		refresh(objs.canvas);
		changeX(NULL, objs.log, 5);
		nanosleep(&(struct timespec){.tv_nsec=0, .tv_sec=1}, NULL);
	}
	if(won) {
		for(unsigned int i = 0; i < 5; i++) {
			render(objs.canvas, 3, LTOBJS{objs.tCanvas, objs.log, objs.splashCityline});
			refresh(objs.canvas);
			changeX(NULL, objs.log, 5);
			nanosleep(&(struct timespec){.tv_nsec=0, .tv_sec=1}, NULL);
		}
	} else {
		for(unsigned int i = 0; i < 5; i++) {
			render(objs.canvas, 3, LTOBJS{objs.tCanvas, objs.log, objs.splashCityline});
			refresh(objs.canvas);
			changeY(NULL, objs.log, -4);
			nanosleep(&(struct timespec){.tv_nsec=0, .tv_sec=1}, NULL);
		}
	}
	render(objs.canvas, 4, LTOBJS{objs.tCanvas, objs.log, objs.splashCityline, objs.title});
	refresh(objs.canvas);
}
//}}


void moveMatch(const MatchStatus *const matchStatus, Move *const move) {
	*move = (Move){.amount=1, .tower=(matchStatus->tower1Height < TOWERSMAXHEIGHT ? true : false)};

	moveLayout(matchStatus);
	while(1) {
		switch(getLastKeyPress(false)) {
			case 'A':
				if(move->amount+(move->tower ? matchStatus->tower1Height : matchStatus->tower2Height) < TOWERSMAXHEIGHT) {
					move->amount++;
				}
				break;
			case 'B':
				if(move->amount > 1) {
					move->amount--;
				}
				break;
			case 'D':
				if(matchStatus->tower1Height < TOWERSMAXHEIGHT) {
					move->tower = true;
				}
				move->amount = 1;
				break;
			case 'C':
				if(matchStatus->tower2Height < TOWERSMAXHEIGHT) {
					move->tower = false;
				}
				move->amount = 1;
				break;
			case '\n':
				return;
		}
		printPreview(matchStatus, move);
		refresh(objs.tCanvas);
		nanosleep(&(struct timespec){.tv_nsec=400000000, .tv_sec=0}, NULL);
	}
}

void playMatch(const ClientMatch *const clientMatch) {
	MatchStatus matchStatus;
	Move move = {.amount=1};

	playLayout(clientMatch);
	do {
		breakIfFalse(receiveStruct(clientMatch->clientSck, &matchStatus))
		if(matchStatus.tower1Height == TOWERSMAXHEIGHT && matchStatus.tower2Height == TOWERSMAXHEIGHT) {
			move.amount = 0;
			break;
		}
		if(matchStatus.turn == clientMatch->playerA.id) {
			rawMode(true);
			moveMatch(&matchStatus, &move);
			rawMode(false);
			breakIfFalse(sendStruct(clientMatch->clientSck, &move))
		} else {
			opponentLayout();
			refresh(objs.canvas);
		}
	} while(1);
	if(move.amount) {
		disconnection();
		refresh(objs.canvas);
	} else {
		endgame(matchStatus.turn != clientMatch->playerA.id);
	}
}

void initializeMatch(ClientMatch *const clientMatch) {
	logSection(1);
	logWrite("waiting for opponent", 2);
	refresh(objs.canvas);

	sendStruct(clientMatch->clientSck, &clientMatch->playerA);
	receiveStruct(clientMatch->clientSck, &clientMatch->playerB);
	clientMatch->playerA.id = !clientMatch->playerB.id;

	logSection(2);
	logString("match started with %.19s", clientMatch->playerB.name)
	logWrite("press any key to continue", 1);
	refresh(objs.canvas);
	getch();
}

void connectToServer(ClientMatch *const clientMatch) {
	SckAttr bindedSckAttr;
	initializeSocket(&clientMatch->clientSck);
	initializeSocketAttr(&bindedSckAttr, SCK_PATH);

	logSection(0);
	gotoXY(NULL, objs.log, xPosition(objs.prompt), yPosition(objs.prompt)+3);
	logWrite("trying to connect", 1);
	refresh(objs.canvas);

	if(!connectSocket(clientMatch->clientSck, &bindedSckAttr)) {
		logWrite("failed to connect to server", 0);
		refresh(objs.canvas);
		exit(1);
	}

	logWrite("connected", 2);
}

void askName(ClientMatch *const clientMatch) {
	splashLayout();
	refresh(objs.canvas);

	moveCursor(10, 5);
	scanf("%19s", clientMatch->playerA.name);
	printName(clientMatch->playerA.name);
}

int main(/*int argc, const char *argv[]*/) {
	ClientMatch clientMatch;
	Obj staticObjs[8];
	objs = (Objs){
			.canvas=&staticObjs[0],
			.title=&staticObjs[1],
			.splashTowers=&staticObjs[2],
			.splashCityline=&staticObjs[3],
			.prompt=&staticObjs[4],
			.log = &staticObjs[5],
			.tCanvas=&staticObjs[6],
			.towerSection=&staticObjs[7]
	};
	objLoader();
	clearScreen();
	askName(&clientMatch);
	echo(false);
	connectToServer(&clientMatch);
	initializeMatch(&clientMatch);
	playMatch(&clientMatch);

	closeSocket(&clientMatch.clientSck);
	objUnloader();
	echo(true);
	discardInput();

	return 0;
}
