# Comandi Linux
* `pwd` stampa il percorso della working directory
* `cd [p]` cambia la working directory al percorso `p`, che può essere relativo o assoluto 
* `ls` stampa il contenuto della working directory
  * `-l` stampa i permessi e altre info di ogni file nella working directory
  * `-a` stampa anche i file nascosti
* `mkdir [dir]` crea una directory `dir`
* `touch [file]` crea un file `file`
* `cp [p1] [p2]` copia un file da `p1` a `p2`
  * `-r` copia ricorsivamente tutti i contenuti di `p2`
  * `-l` copia `p1` come hardlink
  * `-s` copia `p1` come symlink
* `mv [p1] [p2]` sposta il file da `p1` a `p2`\
(`mv` e `cp` possono avere come parametri più file da copiare in una cartella - `mv [p1a] [p1b] [p1c] [p2]`)
* `chmod [nnn] [p]` cambia i permessi del file nel percorso `p`\
  i tre numeri `nnn` rappresentano rispettivamente i permessi per l'utente proprietario (`owner`) di `p`, il gruppo a cui appartiene `p`, e tutti gli altri utenti\
  ogni `n` rappresenta una bitmask di 3b `rwx` convertita in numero decimale, ad esempio, per assegnare `rwx r-x --x` (111 101 001) scriveremmo `chmod 751 [p]`
* `cat {p1a...}` stampa uno o più file, per reindirizzare l'output su file si può usare `cat [p1a...] > [p2]`
* `ln <p1> <p2>` crea un hardlink `p2` a `p1`; crea una entry `p2` che punta allo stesso INode di `p1`
  * `-s` crea un simlink invece che un hardlink; un file `p2` che punta al file `p1`
* `echo [str]` stampa la stringa `str`
* `cut {str}` taglia e stampa una porzione della stringa `str` - `cut lorem&ipsum&dolor -d'&' -f2` -> `ipsum`
  * `-d'[c]' -fn` usa `c` come carattere separatore
  * `-f[n]` stampa solo il campo numero `n`
* `tr [set1] {set2}` sostituisce le occorrenze dei caratteri in `set1`, con i corrispondenti in `set2` - `tr A-Z a-z`, sostituisce le lettere maiuscole con quelle minuscole
  * `-s` (squeeze) sostituisce ogni sequenza di un carattere in `set1` ripetuto, con una singola occorrenza di quel carattere - `tr -s ' '` elimina tutti gli spazi multipli consecutivi
* `ps` stampa la lista dei processi generati dalla shell
  * `-u` (userlist) mostra il nome utente effettivo (l'utente le cui autorizzazioni di accesso ai file sono utilizzate dal processo)
  * `-e\A` (everything) mostra tutti i processi
  * `-l` (long format) mostra più informazioni, ad esempio lo user ID (UID)
* `tail [p]` stampa le ultime 10 righe di `p`
  * `-n [k]` stampa le ultime `k` righe di `p`
  * `-n +[k]` stampa a partire dalla riga `k`
* `du {p}` (disk usage) stampa il numero di blocchi di memoria che occupano gli elementi `p`
  * `-h` (hooman) misura in kB/mB/gB/..., al posto che a blocchi
  * `-b` (byte) misura in byte
  * `-k` (kB) misura in kB
  * `-m` (mB) misura in mB
  * `-c` accoda la dimensione totale
* `find {p}` stampa il contenuto di `p` ricorsivamente, segue i link
  * `-name '[regex]'` filtra i nomi degli elementi in base a `regex`, non cerca all'interno dei file.
* `alias` mostra gli alias attivi
  * `[a]="[b]"`  crea un alias `a` per il comando `b`, vanno usati gli escape code in `b`
* `unalias [a]` elimina l'alias `a`
  * `-a` rimuove tutti gli alias
* `wc {p}` stampa il numero di righe, parole, e caratteri in `p`
  * `-c` mostra numero di byte
  * `-l` numero di righe
  * `-w` mostra solo numero di parole
  * `-m` numero di caratteri
  * `-L` lunghezza della riga più lunga
* `grep [regex] {p}` cerca le occorrenze in `p` in base a `regex`\
  * sintassi di `regex`:
    * `^` (B) inizio della linea
    * `$` (B) fine della linea
    * `\<` (B) inizio di una parola
    * `\>` (B) fine di una parola
    * `.` (B) un singolo carattere (qualsiasi)
    * `[str]` (B) un qualunque carattere in `str`
    * `[^str]` (B) un qualunque carattere non in `str`
    * `[a-z]` (B) un qualunque carattere tra `a` e `z`
    * `\` (B) inibisce l'interpretazione del metacarattere che segue
    * `*` (B) zero o più ripetizioni dell'elemento precedente
    * `+` (E) una o più ripetizioni dell'elemento precedente
    * `?` (E) zero od una ripetizione dell'elemento precedente
    * `{j,k}` (E) un numero di ripetizioni compreso tra `j` e `k` dell'elemento precedente
    * `s|t` (E) l'elemento `s` oppure l'elemento `t`
    * `(exp)` (E) raggruppamento di `exp` come singolo elemento
  * `-E` abilita la sintassi estesa per `regex`
  * `-w` richiede il match per parole intere
  * `-i` (insensitive) abilita la case insensitiveness
  * `-n` mostra il numero della riga
  * `-r` (recursive) ricerca ricorsiva, permette di passare una cartella come `p`
  * `-l` (list) mostra i file che contengono `regex`
  * `-c` (count) mostra i file che contengono `regex` e numero di occorrenze
  * `-B [n]` (before) mostra anche le `n` righe precedenti
  * `-A [n]` (after) mostra anche le `n` righe successive
  * `-C [n]` (context) mostra anche `n` righe tra precedenti e successive
* `sed [actions] {p}`
  * sintassi di `actions`:
    * 
* `set`
* `history` stampa la cronologia dei comandi