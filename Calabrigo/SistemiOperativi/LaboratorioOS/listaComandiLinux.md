# Comandi Linux
* `pwd` stampa il percorso della working directory
* `cd percorso`  mi sposto nel file system, il percorso può essere relativo o assoluto 
* `ls` stampa il contenuto della working directory
  * `-l` stampa i permessi e altre info di ogni file nella working directory
  * `-a` stampa anche i file nascosti
* `mkdir dir` crea una directory 'dir'
* `touch file` crea un file 'file'
* `cp p1 p2` copia un file dal percorso 1 al percorso 2
  * `-r` copia la cartella e tutto il suo contenuto dal percorso 1 al percorso 2
* `mv p1 p2` sposta il file nel percorso 1 al percorso 2\
(mv e cp possono avere come parametri più file da copiare in una cartella - mv f1 f2 f3 d1)
* `chmod xxx p1` cambia i permessi del file nel percorso p1, i permessi sono rispettivamente per utente che ha creato il file, gruppo a cui appartiene il file e tutti gli altri utenti: rwx rwx rwx. Ogni rwx può essere scritto come un numero in base 2 a tre cifre, e quindi rwx = 111 = 7, se volessi dare solo permesso di esecuzione a tutti farei: `chmod 111 file`
* `cat` serve per vedere il contenuto di un file su terminale, si può usare cat anche per scrivere su file in questo modo: `cat > file`
* `ln f2 f2_new` (hard link) il file f2_new ha lo stesso INode di f2. Se due file hanno lo stesso INode vuol dire che sono lo stesso file, se ne modifico uno, mpodifico anche l'altro.
* `echo stringa` stampo su terminale la stringa 'stringa'
* `cut stringa -d'char' -fn` taglio e stampo una porzione della stringa 'stringa'. -d'char' vuol dire che suddividerò in campi la mia stringa in base al carattere 'char', esempio ciao&sono&tizio verrà suddiviso in 3 campi. Per accedere ai campi poi uso -fn, dove n è un numero intero, esempio `cut stringa -d'char' -f2` sulla stringa"ciao&sono&tizio" darà come risultato "sono".
* `tr` sostituisci le occorrenze del primo campo, con quelle del secondo. Esempio: `tr A-Z a-z`, sostituisce le lettere maiuscole con quelle minuscole
  * `-s` tr prende in input un carattere, ed elimina tutte le occorrenze consecutive di quel carattere tranne una. Esempio: `tr -s ' '` elimina tutti gli spazi tranne uno
* `ps` stampa la lista dei processi generati dalla shell
  * `-u` stampa più informazioni, tra cui il nome utente
  * `-el` stampa tutti i processi, con aggiunto lo user ID (UID)
* `tail file -n` stampa le ultime n righe del file, dove n è un numero
  * `tail file -n +k` stampa tutte le righe del file (dove n è il numero di righe) meno k righe in testa.
* `du` stampa il numero di blocchi di memoria che occupa ogni directory
  * `-h` stampa la misura in KB/MB/GB/..., al posto che a blocchi
* `find p1 -name '*.c'` cerca tutti i file e directory da p1 ricorsivamente che finiscano con '.c'. Non cerca all'interno di file.
* `grep`
* `sed`
* `set`
* `alias`