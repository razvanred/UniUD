# NIMTowers

`NIMTowers` is a version of the strategy game NIM. The game is inspired by the achievement of Philippe Petit, who, in 1974, walked on a rope suspended at more than 400 metres above the ground, between the newborn towers of the World Trade Center. The goal is to complete the towers allowing your funambulist to make his walk between the two towers. At each round a player will choose a tower and how many floors to build on that tower. By rules the player can build only on one tower per round, he cannot build less than 1 floor and he cannot build higher than the tower's height limit.


At the beginning of a match each tower starts with a random initial height between 4 and 13 floors, 2 floors less than the height limit. The beginner is also chosen randomly. The match ends when both towers reach the height of 15 floors and the winner is the player who buils the last floor. The winner will see his high-wire walker complete his walk between the towers, the loser will see him fall half way through the walk.

---
## nimServer

The `nimServer` program is responsible for managing the matches. It accepts the players and starts their match, it arbitrates victory and ends the match when either a player wins or he disconnects from the server.

### Starting and matchMaking

When launched, `nimServer` creates a UNIX domain socket and binds it to an address in the file system, at the path specified by the macro `SCK_PATH` defined in the `common.h` file. Before waiting for players the programm starts the thread [`undertaker`](#undertaker) passing to it some synchronization variables and the pointer to a buffer inside of a `Synchronized` struct. This thread will take care of ending the matches.

Now the main thread can start waiting for players. When the first player connects the program will wait for a second one. The server will receive a `Player` struct from each player and start a match between the two on a separate [thread](#matchRoutine). The main thread will also pass to the match thread a `ServerMatch` struct containing a generated thread id used for logging, the structs of the two player and and their sockets, and a copy of the `Synchronized` struct used to synchronize the interaction with the `undertaker` thread.

At this point the main thread can get back to waiting for other two players. If the connection is aborted by the client while accepting the server will wait for another player. Other errors that can occur are caused by system limitation and will cause the program to exit.

To send messages to the clients the `MSG_NOSIGNAL` flag is set to prevent sending errors can exit the program, allowing for error handling (ending the match).

### matchRoutine

`matchRoutine` is the function that match threads run. When it starts matchRoutine assigns each player an id and sends each player name and id of their opponent (the `Player` struct). Now the thread will generate the initial height of the towers and choose which player will move the first round.

For each round the thread will communicate both players the current heights and the id of this round's player. The thread will then wait for a player's move. Now it can check if the move is valid and either pass the turn to the other player or wait for another move from the same player (a player cannot skip rounds).

The thread will stop looping when either the match is finished or a player disconnected causing an error receiving or sending the messages. Then the thread will pass the [`undertaker`](#undertaker) thread it's `serverMatch` struct and exit. The undertaker will take care of collecting the exit status, closing sockets and freeing allocated memory.

### undertaker

The `undertaker` will wait for match [threads](#matchRoutine) to terminate to close their sockets and free their memory.

Before exiting, every match thread will lock on a mutex (`matchesLock`) to prevent more threads trying to exit simultaneously, then a second mutex (`undertakerLock`) and a cond variable (`undertakerCond`) are used to regulate the access to a buffer in which the match thread will store the pointer to its `ServerMatch` struct. This mutexes, cond and the pointer to the buffer are stored in the (`Synchronized`) struct passed to every thread. When (again using `undertakerLock` and `undertakerCond`) the undertaker thread has access to the struct stored in the buffer, it can proceed in joining with the match thread using the id in the struct, closing the players' sockets and freeing the struct. At this point the undertaker will unlock `matchesLock` allowing other match threads to access the buffer and will wait for another thread to finish its work.

---
## nimClient

The `nimClient` program allows players to connect to a `NIMTowers` server to play a match, and provides the user with an "ANSI art" interface.

### communication with server

When the program is launched, it asks the player for a name (Max length 19 characters + 1 terminator). Then the program connects to the server using a socket and the address defined by the macro `SCK_PATH`. The name and a placeholder id are sent to the server. When the match start, the client will receive the opponents name and his actual id from the server. There are only two possible ids (true/false) so the players id is calculated from the opponents id.

At each round the client will receive a struct (`matchStatus`) containing the current heights of the towers and the id of who is making the move . If it is the player's turn, the program will ask the player for the move, and send it to the server in a struct (`move`) containing an identifier of the tower, and the number of new floors. If it is the opponent turn, the client will prompt a waiting screen and wait for the opponent's move. When a player sends an invalid move, the server will simply send the same status to both clients.

When the heights contained in `matchStatus` are both 15 the match is over, and the status will contain the id of the losing player. At this point the client will stop looping and will close the socket and inform the user of his either victory or defeat.

When an error occurs while communicating with server the program will stop looping, close the socket, and exit. Also this time the `MSG_NOSIGNAL` flag is used to allow error handling.

To check if the loop stopped because of an error or because of the end of the match, the amount of floor for the last move is used as a flag. Normally the amount will always be greater than 0, so when the match ends, before breaking the loop, the amount will be set to 0.

### Graphichs

The program offers an "ANSI art" interface. A text-based graphic library, called [`VisualT`](https://github.com/Lucide/VisualT) and written by Riccardo Cavasin, was used for the realization of the interface. This library allowed us to import sprites for every object of the interface (title, background images, prompts, etc), and to easily position them on the screen.

The towers are printed on the screen by stamping the same section of the tower  (`towerSection`) each time changing its vertical position. The "preview" of the new floors is printed in the same way, but the blinking effect is obtained by switching the section sprite at every refresh of the screen.  In order to get a refresh rate [non-blocking input](#Input) is used.

The animation for the winning/loosing screen is obtained by printing the entire towers and the line between them, then updating the position of the funambulist’s sprite at every refresh.

The sprites are designed using the program [REXPaint](https://www.gridsagegames.com/rexpaint/), a powerful and user-friendly ASCII art editor.

### Input

Non-blocking input is necessary to get a refresh rate, it allows to check input at every frame without waiting for the user to give an input. This behaviour is obtained by checking if an input is present before trying to read it, and only read it if it is actually present. It is also necessary to have the terminal not to print the input and not to wait for a new line when reading a character from input. The `termios`'s and `ioctl`'s functions were used to control the terminal's behaviour. Another problem was to discard inputs given when the user was waiting for his opponent, or else at the next turn those inputs could lead to an unwanted move. This was also dealt with using termios an ioctl.

---
## Environment information

* `tested operating systems`:
    * WSL 1
    * Ubuntu
* `compiler`: gcc