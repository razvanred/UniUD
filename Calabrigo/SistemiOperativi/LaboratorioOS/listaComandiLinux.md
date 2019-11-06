# Comandi Linux
* `pwd` stampa il percorso della working directory
* `ls` stampa il contenuto della working directory
  * `-l` (long) stampa i permessi e altre info di ogni file nella working directory
  * `-a` (all) stampa anche i file nascosti
* `du {p}` (disk usage) stampa il numero di blocchi di memoria che occupano gli elementi `p`
  * `-h` (hooman) misura in kB/mB/gB/..., al posto che a blocchi
  * `-b` (byte) misura in byte
  * `-k` (kB) misura in kB
  * `-m` (mB) misura in mB
  * `-c` accoda la dimensione totale
* `cd [p]` cambia la working directory al percorso `p`, che può essere relativo o assoluto 
* `mkdir [dir]` crea una directory `dir`
* `touch [file]` crea un file `file`
* `find {p}` stampa il contenuto di `p` ricorsivamente, segue i link
  * `-name '[regex]'` filtra i nomi degli elementi in base a `regex`, non cerca all'interno dei file.
  * `-print` stampa i file trovati
* `chmod [nnn] [p]` cambia i permessi del file nel percorso `p`\
  i tre numeri `nnn` rappresentano rispettivamente i permessi per l'utente proprietario (`owner`) di `p`, il gruppo a cui appartiene `p`, e tutti gli altri utenti\
  ogni `n` rappresenta una bitmask di 3b `rwx` convertita in numero decimale, ad esempio, per assegnare `rwx r-x --x` (111 101 001) scriveremmo `chmod 751 [p]`
  * `-R` (recursive) applica ricorsivamente
* `chown [user]` assegna `user` come utente di `p`
  * `:{group} [p]` assegna `group` come gruppo di `p`
  * `-R` (recursive) applica ricorsivamente
* `cp [p1] [p2]` copia un file da `p1` a `p2`
  * `-r` (recurse) copia ricorsivamente tutti i contenuti di `p2`
  * `-l` (link) copia `p1` come hardlink
  * `-s` (symlink) copia `p1` come symlink
* `mv [p1] [p2]` sposta il file da `p1` a `p2`\
(`mv` e `cp` possono avere come parametri più file da copiare in una cartella - `mv [p1a...] [p2]`)
* `ln [p1] [p2]` crea un hardlink `p2` a `p1`; crea una entry `p2` che punta allo stesso INode di `p1`
  * `-s` (symlink) crea un simlink invece che un hardlink; un file `p2` che punta al file `p1`
* `echo [str]` stampa la stringa `str`
* `cat {p1...}` stampa uno o più file, per reindirizzare l'output su file si può usare `cat [p1a...] > [p2]`
* `uniq {p}` elimina le ripetizioni adiacenti delle righe in `p`
* `tail [p]` stampa le ultime 10 righe di `p`
  * `-n [k]` stampa le ultime `k` righe di `p`
  * `-n +[k]` stampa a partire dalla riga `k` esclusa
* `head [p]` stampa le prime 10 righe di `p`
  * `-n [k]` stampa le prime `k` righe di `p`
  * `-n -[k]` stampa a salire dalla riga `k` esclusa
* `more {p}` stampa una pagina di `p` e poi una riga per volta
  * `-[n]` usa `n` come dimensione della pagina
  * `+[n]` mostra a partire dalla linea `n` inclusa
* `cut {str}` taglia e stampa una porzione della stringa `str` - `cut lorem&ipsum&dolor -d'&' -f2` -> `ipsum`
  * `-d'[c]' -fn` (delimiter) usa `c` come carattere separatore
  * `-f[n]` (field) stampa solo il campo numero `n`
* `paste [p1...]` affianca riga per riga i `p` separati da `TAB`
  * `-d "str"` usa a rotazione i caratteri di `str` come separatori
* `wc {p}` stampa il numero di righe, parole, e caratteri in `p`
  * `-c` (count) mostra numero di byte
  * `-l` (lines) numero di righe
  * `-w` (words) mostra solo numero di parole
  * `-m` numero di caratteri
  * `-L` (length) lunghezza della riga più lunga
* `grep [regex] {p}` cerca le occorrenze in `p` in base a `regex`
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
  * `-E` (extended) (equivalente a `egrep`) abilita la sintassi estesa per `regex`
  * `-w` (words) richiede il match per parole intere
  * `-i` (insensitive) abilita la case insensitiveness
  * `-n` mostra il numero della riga
  * `-r` (recursive) ricerca ricorsiva, permette di passare una cartella come `p`
  * `-l` (list) mostra i file che contengono `regex`
  * `-c` (count) mostra i file che contengono `regex` e numero di occorrenze
  * `-B [n]` (before) mostra anche le `n` righe precedenti
  * `-A [n]` (after) mostra anche le `n` righe successive
  * `-C [n]` (context) mostra anche `n` righe tra precedenti e successive
  * `-F` (fixed) (equivalente a `fgrep`) cerca una lista di stringhe in `regex` separate da `|` (non accetta regex)
* `tr [set1] {set2}` sostituisce le occorrenze dei caratteri in `set1`, con i corrispondenti in `set2` - `tr A-Z a-z`, sostituisce le lettere maiuscole con quelle minuscole
  * `-s` (squeeze) sostituisce ogni sequenza di un carattere in `set1` ripetuto, con una singola occorrenza di quel carattere - `tr -s ' '` elimina tutti gli spazi multipli consecutivi
* `sed "[action1;action2;...]" {p1...}` esegue le `action`s consecutivamente per ogni riga di ogni `p`
  * sintassi di `action`:
    * `s/[regex]/[str]/` sostituisce la prima occorrenza di `regex`  con `str`
      * `g` (global) ripete per tutte le righe
    * `[n\/regex/]/[command]` esegue `command` sulla riga `n`\sulle righe che contengono `regex`
      * sintassi di `command`
        * `d` (delete) elimina
        * `q` (quit) chiude il programma
        * `p` (print) stampa
  * `-i{suffix}` (in-place) modifica direttamente `p` o una copia con suffisso `suffix`
  * `-s` (separate) elabora i file separatamente
  * `-f [p2]` (file-script) prende le `action`s da `p2`
* `date` stampa la data
* `cal` stampa il calendario del mese
  * `-y` (year) stampa il calendario dell'anno corrente
  * `[n]` stampa il calendario dell'anno `n`
  * `-m [month]` stampa il calendario di `month`
* `history` stampa gli ultimi 500 comandi
  * `[n]` stampa gli ultimi `n` comandi
  * sintassi per l'espansione della history: `[command]:{filter}`
    * sintassi di `command`
      * `![n]` richiama l'`n`-esimo comando
      * `!-[n]` richiama l'`n`-ultimo comando
      * `!!` richiama l'ultimo comando
      * `!?[str]` richiama l'ultimo comando che contiene `str`
      * `![str]` richiama l'ultimo comando che inizia per `str`
    * sintassi di `filter`
      * `s/[str1]/[str2]/` sostituisce `str1` con `str2`
      * `[n]` richiama l'`n`-esimo argomento del comando
      * `*` richiama tutti gli argomenti del comando
      * `[n1]-[n2]` richiama gli argomenti del comando da `n1` a `n2`
* `alias` mostra gli alias attivi
  * `[a]="[b]"`  crea un alias `a` per il comando `b`, vanno usati gli escape code in `b`
* `unalias [a]` elimina l'alias `a`
  * `-a` (all) rimuove tutti gli alias
* `bg %[n]` manda l'`n`esimo job in esecuzione in background finché non richiede un input (l'output viene mostrato sul terminale)
* `fg %[n]` riporta l'`n`esimo job in esecuzione in foreground
* `kill  [pid\%n]` invia segnale 15 al processo con quel `pid`\all'`n`esimo job in esecuzione sulla shell (se è sospeso raccoglie il segnale quando torna in esecuzione)
  * `-[signal]` invia il segnale `signal`:
    * `9` termina forzatamente
    * `15` chiede la terminazione
* `jobs` elenca i processi in background sulla shell corrente
* `ps` stampa la lista dei processi generati dalla shell
  * `-u` (userlist) mostra il nome utente effettivo (l'utente le cui autorizzazioni di accesso ai file sono utilizzate dal processo)
  * `-e\A` (everything) mostra tutti i processi
  * `-l` (long format) mostra più informazioni, ad esempio lo user ID (UID)
* `top` visualizza le informazioni di riepilogo del sistema così come un elenco di processi o thread
  * `-b` (batch mode) esegue in modalità batch, utile per ridirezionare l'output
  * `-n [n]` stampa `n` riepiloghi
  * `-d [n]` aggiorna ogni `n` secondi