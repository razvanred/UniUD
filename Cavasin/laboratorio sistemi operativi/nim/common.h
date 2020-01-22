#ifndef NIM__COMMON_H_
#define NIM__COMMON_H_

#include <stdlib.h>
#include <termios.h>
#include <stdbool.h>

typedef struct {
	char name[20];
	bool id;
} Player;

typedef struct {
	unsigned short tower1Height, tower2Height;
	bool turn;
} MatchStatus;

typedef struct {
	bool tower;
	short amount;
} Move;

void initializeRand() {
	srand((unsigned int)&initializeRand);
}

#pragma clang diagnostic push
#pragma ide diagnostic ignored "cert-msc50-cpp"

unsigned int getRand(unsigned int minInc, unsigned int maxInc) {
	return rand()%(maxInc+1-minInc)+minInc; // NOLINT(cert-msc30-c)
}

#pragma clang diagnostic pop

#pragma clang diagnostic push
#pragma ide diagnostic ignored "hicpp-signed-bitwise"

void setNonBlockingInput(bool mode) {
	struct termios tty;

	tcgetattr(STDIN_FILENO, &tty);
	if(mode) {
		tty.c_lflag &= ~(ICANON|ECHO);
		tty.c_cc[VMIN] = 1;
	} else {
		tty.c_lflag |= (ICANON|ECHO);
	}
	tcsetattr(STDIN_FILENO, TCSANOW, &tty);
}

int kbhit() {
	struct timeval tv;
	fd_set fds;

	tv.tv_sec = 0;
	tv.tv_usec = 0;
	FD_ZERO(&fds);
	FD_SET(STDIN_FILENO, &fds);
	select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
	return FD_ISSET(STDIN_FILENO, &fds);
}

int getKeyPress(bool keepEscapes) {
	int c = 0;

	while(kbhit()) {
		c = getchar();
		if(!keepEscapes && c == '\033') {
			getchar();
			c = getchar();
		}
	}
	return c;
}

#pragma clang diagnostic pop

#endif //NIM__COMMON_H_
