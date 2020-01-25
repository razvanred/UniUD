#ifndef NIM__COMMON_H_
#define NIM__COMMON_H_

#include <termios.h>
#include <stropts.h>
#include <asm/ioctls.h>

#define SCK_PATH "/home/nemo/nim/socket"
#define TOWERSMAXHEIGHT 15

// GRAPHICS
#define lPuts(STRING) fputs((STRING), stdout)
#define clearScreen() lPuts("\033[2J")
#define rewindScreen() lPuts("\033[0;0H")
#define moveCursor(X, Y) lPuts("\033[" #Y ";" #X "H")
#define refresh(CANVAS) rewindScreen();  \
                        print((CANVAS), true)
#define breakIfFalse(EXP) if(!(EXP)){  \
                            break;    \
                          }

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
	unsigned short amount;
} Move;

typedef struct Obj Obj;

void initializeRand() {
	srand((getpid()));
}

unsigned int getRand(unsigned int minInc, unsigned int maxInc) {
	return rand()%(maxInc+1-minInc)+minInc; // NOLINT(cert-msc30-c)
}

int getch() {
	struct termios oldt, newt;
	int c;

	tcgetattr(STDIN_FILENO, &oldt);
	newt = oldt;
	newt.c_lflag &= ~(ICANON|ECHO); // NOLINT(hicpp-signed-bitwise)
	tcsetattr(STDIN_FILENO, TCSANOW, &newt);
	c = getchar();
	tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
	return c;
}

unsigned int kbhit() {
	static bool initialized = false;

	if(!initialized) {
		struct termios term;
		tcgetattr(STDIN_FILENO, &term);
		term.c_lflag &= ~(ICANON|ECHO);
		tcsetattr(STDIN_FILENO, TCSANOW, &term);
		setbuf(stdin, NULL);
		initialized = true;
	}
	int bytesWaiting;
	ioctl(STDIN_FILENO, FIONREAD, &bytesWaiting);
	return bytesWaiting;
}

void discardInput() {
	while(kbhit()) {
		getchar();
	}
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

#endif //NIM__COMMON_H_
