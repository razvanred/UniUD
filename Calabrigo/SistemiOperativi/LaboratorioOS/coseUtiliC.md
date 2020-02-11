# Cose Utili C

### librerie
#include < stdio.h >: per printf e scanf
* getChar() // prende un carattere dal terminale
* putChar(char c) // stampa un carattere sul terminale
* printf("%d",num) // stampa un numero %d, carattere %c, float %f ecc.
* scanf("%d",$num) // legge un valore dallo std input. Ritorna il numero di argomenti letti.
* FILE *f = fopen("path", mode). Apre un file.
  * "r" (lettura)
  * "w" (scrittura)
  * "a" (append)
* int fclose(FILE *fp). Chiude un file.
* fscanf(fd, "ciao");
* sscanf(fd, "%d", &var);
* feof(fd);
* fseek(fd, offset, SEEK_?);
  * SEEK_SET: Inizio del file
  * SEEK_CUR: Posizione attuale
  * SEEK_END: Fine del file
Esempio(più complesso):
    * float real = 0, float imag = 0;
    * scanf("( %f , %f )", &real, &imag);
* sscanf(stringa, "%d", &var); // come scanf, ma al posto di leggere da std in, legge da stringa.
Esempio:
    * char *valori="(3.14, 0)";
    * float real = 0, imag = 0;
    * sscanf(valori, "(%f, %f)", &real, &imag);
    * printf("c = %f + i%f\n", real, imag);
* sprintf(stringa, "%d", var); // come printf, ma scrive su stringa al posto di stdout.\
Esempio:
    * char valori[42] = "";
    * float real = 3.14, imag = 0;
    * sprintf(valori, "(%f, %f)", real, imag);


#include < stdbool.h >: per usare bool

#include < string.h >: per usare le funzioni delle stringhe
* strcpy(s1, s2). Copies string s2 into string s1.
* strlen(s1). Returns the length of string s1.
* strcat(s1, s2). Concatenates string s2 onto the end of string s1.
* strcmp(s1, s2). Returns 0 if s1 and s2 are the same; less than 0 if s1 < s2; greater than 0 if s1 > s2.
* strchr(s1, ch). Returns a pointer to the first occurrence of character ch in string s1.
* strstr(s1, s2). Returns a pointer to the first occurrence of string s2 in string s1.

#include< stdlib.h >: per usare malloc
* malloc(unsigned n). Alloca una zona contigua in memoria, il parametro n indica il numero di byte.\
Esempio: int *vec = malloc(5*sizeof(int)); // per creare un vettore di 5 posizioni.
    * La funzione accetta come argomento il numero di byte di memoria di cui si ha bisogno.
    * Alloca una zona di memoria contigua della dimensione richiesta e restituisce un puntatore all’inizio di tale zona.
    * Il tipo di ritorno void * è un puntatore ad un tipo qualsiasi.
* free(vec). Libera la zona di memoria occupara da vec.
* realloc(vec, unsigned n). Modifica la zona di memoria di vec che era stata allocata da un malloc, e la sua nuova dimensione sarà il secondo parametro di realloc.
* 
#include< unistd.h >: per usare le funzioni posix
* read(fd, &val, sizeof(int)).
* write(fd, &val, sizeof(int)).
* lseek(fd, offset, SEEK_?). Come per fseek.
* close(fd).
* open(fd, O_CREAT | O_RDONLY | ecc.).
* int chdir(const char *path). Cambia la directory di lavoro corrente da quella attuale (ovvero ./), a quella indicata nel path. Cosi la prossima volta che scriverò ./ sarà come scrivere path.
* int pipe(int *filedes). Crea una pipe. Filedes deve puntare ad un array di due interi, che conterranno i file descriptor dei due capi della pipe.
    * Il primo, filedes[0], serve a leggere dalla pipe.
    * Il secondo, filedes[1], serve a scrivervi.\
    * N.B: quando creo la pipe non devo inserire io in filesdes i pid dei processi figlio e padre, invece filesdes verrà passato come vettore vuoto (int filesdes[2]  {}), che verrà riempito dal metodo: pipe(filesdes), con gli fd del lato lettura della pipe (filedes[0]) e scrittura (filesdes[1]).
* getpid(). Ritorna il pid del processo corrente.
* getppid(). Ritorna il pid del processo padre.

#include< fcntl.h >: per usare O_RDONLY, O_CREAT, , O_RDWR, ecc.

#include< sys/wait.h >: per usare wait
* wait(NULL). Il processo che chiama wait mette in pausa la sua esecuzione finchè non avrann terminato tutti i figli.

#include< stdlib.h >. Serve per usare NULL.

#include < sys/types.h >:serve per i socket\
#include < signal.h >:serve per i socket
* alarm(unsigned int secs). Manda un segnale SIGALRM dopo un tempo specificato (secs).
* signal(sig, nomeFunzione). Il processo che usa signal fa in modo che se dovesse arrivargli il segnale sig (specificato nella funzione), al posto di terminare il processo verrà eseguita la funzione nomeFunzione. Esempio: 
  * void fun(){printf("do not terminate");}
  * int main(){
  * signal(SIGINT, fun);//setto fun come funzione da eseguire in caso di ricezione di SIGINT
  * return 0;}
* int kill(int pid, int sig). Serve a killare un processo. Devi dargli in input un pid e un segnale sig:
    * 1 SIGHUP terminal line hangup
    * 2 SIGINT interrupt program
    * 3 SIGQUIT quit program
    * 4 SIGILL illegal instruction
    * 5 SIGTRAP trace trap
    * 6 SIGABRT abort program (formerly SIGIOT)
    * 7 SIGEMT emulate instruction executed
    * 8 SIGFPE floating-point exception
    * 9 SIGKILL kill program
    * 10 SIGBUS bus error
    * 11 SIGSEGV segmentation violation
    * 12 SIGSYS non-existent system call invoked
    * 13 SIGPIPE write on a pipe with no reader
    * 14 SIGALRM real-time timer expired
    * 15 SIGTERM software termination signal
    * 16 SIGURG urgent condition present on socket
    * 17 SIGSTOP stop (cannot be caught or ignored)
    * 18 SIGTSTP stop signal generated from keyboard
    * 19 SIGCONT continue after stop
    * 20 SIGCHLD child status has changed
    * 21 SIGTTIN background read from control terminal
    * 22 SIGTTOU background write to control terminal
    * 23 SIGIO I/O is possible on a descriptor
    * 24 SIGXCPU cpu time limit exceeded
    * 25 SIGXFSZ file size limit exceeded
    * 26 SIGVTALRM virtual time alarm
    * 27 SIGPROF profiling timer alarm
    * 28 SIGWINCH Window size change
    * 29 SIGINFO status request from keyboard
    * 30 SIGUSR1 User defined signal 1
    * 31 SIGUSR2 User defined signal 2

#include < sys/types.h >: server per usare le funzioni dei socket
#include< sys/socket.h >: server per usare le funzioni dei socket
* int fd = socket(AF_LOCAL, SOCK_STREAM, 0). Crea un socket UNIX_DOMAIN.
### costanti
#define VAR 10: definisco una costante VAR=10

### array e puntatori
In C un array si dichiara facendo: int array[size];\
e poi array[0] = a; array[1] = b; ecc.\
Possiamo dichiarare un array anche con i puntatori facendo int *p = array;(vedi sopra) in questo modo *p punterà ad array. *p = a; p++; *p = b; p++; ecc.\
Se ho un puntatore p, allora:
* il simbolo *p significa il contenuto della variabile puntata da p
* il simbolo p significa l'indirizzo della variabile puntata da p
* il simbolo &p significa l'indirizzo di p.

N.B: ricorda che quando dichiari un puntatore devi fare: int *p; e in questo caso *p non significa il contenuto della variabile puntata da p, ma solo la dichiarazione del puntatore.

### arc e argv
N.B.: come parametri si intendono le stringhe tipo "-s", "-r", ecc., che di solito si usano sui comandi linux.

int main(int argc, char **argv);\
* argc:
    * è il numero di parametri passati.
* argv:
    * Il primo parametro è sempre il nome del programma.
    * I restanti sono disposti in ordine, così come passati dalla shell.
    * L’ultimo elemento (argv[argc]) è NULL.
    * argv[1] è il primo parametro passato dall'utente

### struct
struct point {
float x;
float y;
};\
Esempio di lettura da stdin e scrittura su struct:\
struct point p = { };\
scanf("{ %f , %f }", &p.x, &p.y);

Esempio di struct passata per funzione:\
float abs(struct point p) {\
return sqrt(p.x * p.x + p.y * p.y);}

N.B.: Ricorda che quando dichiari una struct point devi fare: struct point p = ...; (invece fare solo: point p = ...; sarebbe sbagliato).

Sintassi: (*p).x è lo stesso di p->x


### distribuire il codice
* file.h\
    * #ifndef FILE_H__
    * #define FILE_H__
    * int add(int x, int y);\
    * #endif
* file.c\
    * #include "file.h"\
    * int add(int x, int y) {return x + y;}\
* main.c\
    * #include "file.h"\
    * #include< stdio.h >\
    * int main(){\
    * printf("3 + 4 = %d\n", add(3,4));\
    * return 0;}
# processi
int pid = fork(). Crea un processo figlio e ne ritorna il pid.\
Al genitore viene restituito il PID del figlio, mentre al figlio viene restituito 0, in questo modo li posso riconoscere.\
int execl(const char *path, const char *arg0, ...). Serve a cambiare l'eseguibile del processo che chiama questa istruzione, in pratica cambio il processo.
*  L’argomento path è l’eseguibile che si vuole lanciare
* Gli argomenti successivi sono gli argomenti da riga di
comando che si vogliono passare al programma, terminati da
un puntatore nullo.

Esempio:\
int pid = fork();\
if(pid == 0){//se sono il figlio\
    execl("/bin/ls", "ls", "-l", NULL); //sostituisco il mio eseguibile con quello in "/bin/ls".\
}

### Come passare parametri al figlio durante la sua creazione: variabili d'ambiente
int main(int argc, char **argv, char **envp); // envp è un vettore di stringhe, il cui ultimo elemento è NULL.\
Per passare dei parametri al figlio, devo usare: execve(path, argv, envp);\
Esempio:\
int main(int arc, char **argv){\
char *envp[3] = { "var1=valore1", "var2=valore2", NULL };\
execve("./env2", argv, envp);\
perror("execve fallita");\
return 1;}\
N.B: envp deve sempre avere NULL come ultimo elemento.

# socket
Funzioni dei socket:
* socket() Crea il file descriptor di un capo della connessione entrambi.Server e client
* bind() Lega il socket ad un indirizzo specifico server.Solo server
* listen() Blocca il processo in ascolto sul socket server.Solo server
* accept() Accetta una connessione in arrivo server.Solo server
* connect() Connette un socket ad un altro socket in ascolto client.Solo client