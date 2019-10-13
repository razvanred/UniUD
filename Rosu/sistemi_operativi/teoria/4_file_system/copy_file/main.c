/**
 * Razvan Rosu <razvanred.work@gmail.com>
 *
 * Programma di copia file.
 */

#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>

/* puntatore ad un vettore (a sua volta puntatore) */
int main(int argc, char *argv[]); /* Prototipo ANSI */

#define BUF_SIZE 4096 /* usa un buffer di 4096 byte */
#define OUTPUT_MODE 0700 /* bit di protezione per file di output */

int main(int argc, char *argv[]) {

    //il file descriptor in genere è un intero maggiore o uguale a 0

    int in_fd; // file descriptor di input
    int out_fd; // file descriptor di output
    int rd_count; // numero di caratteri scritti tramite la funzione read
    int wr_count; // numero di caratteri scritti tramite la funzione write

    //buffer dove andremo a salvare il contenuto del file di input e da cui andremo a leggere per scrivere sul file di output
    char buffer[BUF_SIZE];

    if(argc != 3){
        exit(1); // numero di argomenti invalido
    }

    in_fd = open(argv[1], O_RDONLY); // apertura del file di input
    if(in_fd < 0) { // se il file descriptor è minore di 0 c'è stato un errore di apertura del file di input
        exit(2);
    }

    out_fd = creat(argv[2], OUTPUT_MODE); // creazione del file di output
    if(out_fd < 0){ // se il file descriptor è minore di 0 c'è stato un errore di creazione del file di output
        exit(3);
    }

    while(true){
        rd_count = read(in_fd, buffer, BUF_SIZE); // legge il contenuto del file di input attraverso il file descriptor
        if(rd_count <= 0){ // può essersi verificato un errore di lettura (se minore di 1) oppure ha finito
            break;
        }
        wr_count = write(out_fd, buffer, rd_count); // scrive il contenuto del buffer sul file di output,
        if(wr_count <= 0){ // se non è stato scritto alcun carattere ci deve essere stato un errore, dato che rd_count in questo punto dovrebbe essere strettamente maggiore di 0
            exit(4);
        }
    }

    // chiusura dei file
    close(in_fd);
    close(out_fd);

    if(rd_count == 0){
        exit(0); // tutte le righe del file di input sono state lette
    } else {
        exit(5); // errore di lettura
    }
}