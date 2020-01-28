#ifndef NIM__COMMON_H_
#define NIM__COMMON_H_

#include <termios.h>
#include <stropts.h>
#include <asm/ioctls.h>

#define SCK_PATH "socket"
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
	return rand()%(maxInc+1-minInc)+minInc; // NOLINT(cert-msc30-c,cert-msc50-cpp)
}

void echo(bool echo) {
	struct termios tAttr;
	tcgetattr(STDIN_FILENO, &tAttr);

	if(echo) {
		tAttr.c_lflag |= ECHO; // NOLINT(hicpp-signed-bitwise)
	} else {
		tAttr.c_lflag &= ~ECHO; // NOLINT(hicpp-signed-bitwise)
	}
	tcsetattr(STDIN_FILENO, TCSANOW, &tAttr);
}

void discardInput() {
	// tell the C library not to buffer any data from/to the stream
	setvbuf(stdin, NULL, _IONBF, 0);
	// discard all unread input in the system buffer
	tcflush(STDIN_FILENO, TCIFLUSH);
}

void rawMode(bool rawMode) {
	static struct termios old;

	if(rawMode) {
		discardInput();
		tcgetattr(STDIN_FILENO, &old);
		struct termios raw = old;
		// disable "canonical" mode
		raw.c_lflag &= ~ICANON; // NOLINT(hicpp-signed-bitwise)
		// in non-canonical mode, we can set whether getc() returns immediately
		// when there is no data, or whether it waits until there is data
		raw.c_cc[VMIN] = 1;
		raw.c_cc[VTIME] = 0;
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
	} else {
		// discard all unread input in the system buffer
		tcflush(STDIN_FILENO, TCIFLUSH);
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &old);
	}
}

// requires unbuffered

unsigned int kbhit() {
	int bytesWaiting;
	ioctl(STDIN_FILENO, FIONREAD, &bytesWaiting);
	return bytesWaiting;
}

int getch() {
	rawMode(true);
	int c = getchar();
	rawMode(false);
	return c;
}

int getLastKeyPress(bool keepEscapes) {
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
