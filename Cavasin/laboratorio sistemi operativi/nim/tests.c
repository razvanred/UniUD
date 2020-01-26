#include <unistd.h>
#include <termios.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

/* call this to change the terminal related to the stream to "raw" state
 * you must restore the state before your program exits, or your user will
 * frantically have to figure out how to type 'reset' blind, to get their terminal
 * back to a sane state.
*/
void toggleRawMode() {
	static bool isRaw = false;
	static struct termios old;

	if(isRaw) {
		// write/discard already buffered data in the C library input buffer
		fflush(stdin);
		// discard all unread input in the system buffer
		tcflush(STDIN_FILENO, TCIOFLUSH);
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &old);
		isRaw = false;
	} else {
		// tell the C library not to buffer any data from/to the stream
		setvbuf(stdin, NULL, _IONBF, 0);
		// write/discard already buffered data in the C library input buffer
		fflush(stdin);
		// discard all unread input in the system buffer
		tcflush(STDIN_FILENO, TCIOFLUSH);
		tcgetattr(STDIN_FILENO, &old);
		struct termios raw = old;
		// disable "canonical" mode, and echo
		raw.c_lflag &= ~(ICANON|ECHO); // NOLINT(hicpp-signed-bitwise)
		// in non-canonical mode, we can set whether getc() returns immediately
		// when there is no data, or whether it waits until there is data
		raw.c_cc[VMIN] = 1;
		raw.c_cc[VTIME] = 0;
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
		isRaw = true;
	}
}

int main() {
	toggleRawMode();

	fputs("nonBlock:",stdout);
	getc(stdin);
	fputs("nonBlock:",stdout);
	getc(stdin);
	fputs("nonBlock:",stdout);
	getc(stdin);

	toggleRawMode();

	fputs("block(enter):",stdout);
	getchar();
	getchar();
	return 0;
}