# Cose Utili C

### librerie
#include < stdio.h >: per printf e scanf
* getChar() // prende un carattere dal terminale
* putChar(char c) // stampa un carattere sul terminale
* printf("%d",num) // stampa un numero %d, carattere %c, float %f ecc.
* scanf("%d",$num) // legge un valore dallo std input. Ritorna il numero di argomenti letti.
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
* open(fd, O_CREAT | O_RDONLY | ecc.)

#include< fcntl.h >: per usare O_RDONLY, O_CREAT, , O_RDWR, ecc.
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



