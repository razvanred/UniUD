#include <stdio.h>
#include <values.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <stdio.h>

typedef struct Obj Obj;

#define RESERVED_THRIDS 0
unsigned int currentThrId = UINT_MAX-1;

void test1() {
	for(unsigned long i = 1; i <= UINT_MAX+1ul; i++) {
		currentThrId = (currentThrId-RESERVED_THRIDS+1)%(UINT_MAX-RESERVED_THRIDS)+RESERVED_THRIDS;
		if(i == 1 || i == 2 || i == UINT_MAX || i == UINT_MAX+1ul) {
			printf("%u\n", currentThrId);
		}
	}
}

int
keypressed() {
	/* These are for ioctl */
	struct sgttyb tty, ntty;
	int ttyset, stat, arg;

	ttyset = 0;

	/*
	 * The TIOCGETP ioctl call gets the tty information structure.
	 * See tty(4) for details about the contents of that structure.
	 */
	stat = ioctl(0, TIOCGETP, &tty);
	if(stat == -1) {
		perror("ioctl");
		return (-1);
	}

	/*
	 * CBREAK is the status flag that controls character by character
	 * input mode.  This if statement checks to see if CBREAK is
	 * already enabled and only enables it if it is not.
	 */
	if(!(tty.sg_flags&CBREAK)) {
		ntty = tty;
		ttyset = (!ttyset);
		/* OR'ing the status bits with CBREAK turns it on. */
		ntty.sg_flags |= CBREAK;
		/* TIOCSETN changes the terminal characteristics, without */
		/* discarding pending data.                               */
		stat = ioctl(0, TIOCSETN, &ntty);
		if(stat == -1) {
			perror("ioctl");
			return (-1);
		}
	}

	/* FIONREAD returns the number of characters of waiting input */
	stat = ioctl(0, FIONREAD, &arg);
	if(stat == -1) {
		perror("ioctl");
		return (-1);
	}

	if(ttyset) {
		/* put the tty characteristics back to their original form */
		stat = ioctl(0, TIOCSETN, &tty);
		if(stat == -1) {
			perror("ioctl");
			return (-1);
		}
	}

	return (arg);
}

int main() {
	compose();
	// test1();

	return 0;
}