# NIMTowers

`NIMTowers` is a reinterpretation of the strategy game NIM, inspired by the achievement of high wire artist [Philippe Petit](https://en.wikipedia.org/wiki/Philippe_Petit), who, in 1974, walked on a tightrope wire between the newborn towers of the World Trade Center, at 400m above the ground.

The goal of the game is to be the first to complete the towers, allowing your funambulist to make his walk on the wire. At each round the player will choose a tower and how many floors to build. By rules the player can build only on one tower per round, and he can't build less than 1 floor, or build higher than the tower's height limit.

At the beginning of a match, each tower starts with a random amount of floors between 4 and 13, out of a maximum height of 15. The player that gets to make the first move is also chosen randomly. The match ends when both towers reach maximum height, so who builds the last floors wins. The winner will see his high-wire walker complete his stunt, the loser will see him fall to his doom.

---
## `nimServer`

The `nimServer` program is responsible for managing matches. It accepts new players and starts new games, besides arbitrating victories and ending matches when either player wins or disconnects.

### startup and matchMaking

When launched, `nimServer` creates a UNIX domain socket and binds it to an address in the file system, at the path specified in the `SCK_PATH` macro in the `common.h` file. The [`undertaker()`](#undertaker) thread is then started, which has the task of disposing of 'dead' matches.

When the first player connects, the program will wait for a second one. Both players will then send a `Player` struct containing their respective `name` (and an uninitialized `id` field), used to create a new `ServerMatch` struct.\
A `ServerMatch` contains:
* a `pthread_t` object
* a generated, printable, thread id (`thrId`)
* two `ServerPlayer` structs, both of which contain a `Player` with the respective socket
* a copy of the `Synchronized` struct, used to synchronize the interactions with the [`undertaker()`](#undertaker) thread
 
The struct is then passed to a new [`matchRoutine()`](#matchRoutine) thread, and the main thread can go back to waiting for two new players. This process is repeated whenever a new match can be created.

If the connection gets interrupted while accepting a new player, the server will resume waiting for another player. Other errors that could occur are assumed to be more serious and unrecoverable (e.g. system errors), thus, the program will just terminate.

The `MSG_NOSIGNAL` flag has been set for the `send()` call to prevent forwarding errors to crash the program, allowing for error handling (e.g. by ending the match).

### `matchRoutine()`

`matchRoutine()` is the function ran by "per-match" threads. When it starts, `matchRoutine()` sets the `id` field of both `Player` structs, and sends them back switched, so both players obtain the `name` and `id` of the opponent (from which they can deduce their own `id`). Next, the function generates the towers' initial height and chooses who'll make the first move.

For each round, the thread will communicate to both players the current towers' heights and the `id` of the moving player. It will then wait for the move to be completed.\
In the event that the received move is not valid, the server will not proceed to the next round (a player cannot skip rounds).

The thread will stop looping when either the match is finished, or a connection error has occurred. Then, it'll wait to write a reference to it's `ServerMatch` on `undertakerBuffer` and exit. The [`undertaker()`](#undertaker) routine will take care of disposing of the 'dead' thread, by closing sockets, and freeing allocated memory.

### `undertaker()`

The `undertaker()` thread waits for [`matchRoutine()`](#matchRoutine) threads to terminate, to close their sockets and free their memory.

Before exiting, every [`matchRoutine()`](#matchRoutine) thread will lock on the `matchesLock` mutex to prevent more than one [`matchRoutine()`](#matchRoutine) from terminating at the same time.\
Then, a second mutex `undertakerLock`, and a cond variable `undertakerCond`, are used to regulate the access to the `undertakerBuffer`, in which a reference to a *dead* [`matchRoutine()`](#matchRoutine)s' `ServerMatch` is stored.\
All this synchronizing-related data is stored in the `Synchronized` struct, available to every relevant thread. When the `undertaker()` thread gets access to the `undertakerBuffer`, it can proceed to join with the *dead* [`matchRoutine()`](#matchRoutine), close the socket in both `ServerPlayer`, and free the `ServerMatch` struct.\
At last, `undertaker()` unlocks `matchesLock`, allowing for other *dying* [`matchRoutine()`](#matchRoutine)s to access `undertakerBuffer`, and the process repeats.

---
## `nimClient`

The `nimClient` program allows players to connect to a `nimServer` and play, and provides the user with an ANSI Art-like interface.

### communication with server

When the program is launched, it asks for a player name (max length 19 characters, stored in a 20B `\0` terminated string inside `Player`). Then, it attempts to connect to the server by using a socket and the address defined in the macro `SCK_PATH`. A partially initialized `Player` struct is then sent to the server, as the `id` has yet to be defined. When the match starts, the client will receive from the server the opponents' `Player`. There are only two possible values for `id` (true/false) so a players' `id` is the opponents' `id` flipped.

At each round, the client will receive a `matchStatus` struct, containing the current heights of the towers, and the `id` of who has to make a move. If it's the player's turn, the program will show the "move user interface", otherwise, it'll show a waiting screen.\
A move is represented by the `Move` struct, which contains a `tower` identifier, and the `amount` of new floors. When a player sends an invalid `Move`, the server will simply resend the same `matchStatus` to both clients.

When `tower1Height` and `tower2Height` in `matchStatus` reach both 15, the match is over, and `turn` will contain the `id` of the losing player (since he should have been the next to make a move). At this point the client will stop looping, close the socket, and it will inform the player of his either victory or defeat.

If an error occurs while communicating with the server, the program will close the socket and exit gracefully. The `MSG_NOSIGNAL` flag still matters since the code is in a shared header.

To track if the main loop stopped naturally or because of a connection error, the `amount` value in `Move` is used as a flag. Normally, `amount` will always be greater than 0, so, to indicate that an abrupt interruption hasn't happened, `amount` is set to 0 when a match concludes correctly.

### Graphics

The program offers an interactive ANSI Art-like interface. In order to compose the UI as quickly as possible, we used a text-based graphic library called [`VisualT`](https://github.com/Lucide/VisualT), previously written by one of us. This library allowed us to import sprites for every object of the interface (title, background images, prompts, etc), and to easily position them on the screen.

The towers are printed on the screen by 'stamping' the same section of the tower (`towerSection`) repeatedly, changing its vertical position each time. The "preview" of the new floors is generated in the same way, but the blinking effect is obtained by switching `towerSection`'s sprite at every refresh of the screen. In order to get a proper 'refresh rate', [non-blocking input](#Input) is used.

We designed the sprites using [REXPaint](https://www.gridsagegames.com/rexpaint/), a powerful and user-friendly ASCII art editor written by Josh Ge.

### Input

Properly handling of non-blocking input was an important part of achieving an input-independent refresh rate. It was ultimately obtained by painfully studying `termios.h` and `stdio.h` APIs.\
We implemented DOS' `kbhit()` function, which checks if any input is pending in the `stdin` buffer. This let us call `getchar()` only if there already was at least a char waiting to be processed (so, in fact making it non-blocking).\
We also implemented the `getch()` function, which disables the `ICANON` flag to not to wait for a new line when reading a character from input.\
One of the hardest task was to create a reliable way to discard inputs from the `stdin` buffer at command, for example when inserted accidentally by the user while the program was idle. This was achieved by disabling `stdin` input buffering each time the terminal's *raw mode* is activated, and then calling `tcflush()`.

---
## Environment information

* `tested operating systems`:
    * WSL 1
    * Ubuntu
* `compiler`: gcc