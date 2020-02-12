# Comandi Linux
* `pwd` stampa il percorso della working directory
* `ls [p]` stampa il contenuto della cartella `p`\il nome del file `p`
  * `-R` [recursive] stampa il contenuto della cartella `p` ricorsivamente
  * `-l` (long) stampa i permessi e altre info di ogni file
  * `-a` [all] stampa anche i file nascosti
  * `-t` (time) ordina per la data di ultima modifica, in ordine decrescente
  * `-d` [directory] se `p` è una cartella stampa solo la cartella e non il suo contenuto
  * `-i` [inode] stampa il numero di inode per ogni elemento
  * `-s` [size] stampa la dimensione in blocchi di ogni elemento
* `du [p]` (disk usage) stampa il numero di blocchi di memoria che occupano gli elementi `p`
  * `-h` [human-readable] misura in kB\mB\gB\\..., al posto che a blocchi
  * `-b` [bytes] misura in byte
  * `-k` [block-size=1K] misura in kB
  * `-m` [block-size=1M] misura in mB
  * `-c` [total] accoda la dimensione totale
* `cd [dir]` sposta la working directory su `dir` 
* `mkdir [dir]` crea `dir`
* `rmdir [dir]` elimina `dir`
* `rm [p]` elimina il file `p`
  * `-r` [recursive] elimina la cartella `p` ricorsivamente
  * `-f` [force] disabilita la richiesta di conferma
  * `-i` (interactive) richiede conferma per ogni elemento
* `touch [file]` crea il file `file`
* `find [dir]` stampa il contenuto di `dir` ricorsivamente, segue i link
  * `-name '[regex]'` filtra i nomi degli elementi in base a `regex`, non cerca all'interno dei file
  * `-exec [command] {} ;` esegue `command` per ogni file trovato, le parentesi graffe `{}` vengono sostituite con il nome del file. Potrebbe servire fare l'escaping di `;` o delle `{}`
  * `-execdir [command] {} ;` esegue `command` per ogni cartella trovata, le parentesi graffe `{}` vengono sostituite con il nome della cartella. Potrebbe servire fare l'escaping di `;` o delle `{}`
  * `-exec [command] {} +` esegue `command` per ogni file trovato, le parentesi graffe `{}` vengono sostituite con il nome di tutti i file trovati. Potrebbe servire fare l'escaping di `;` o delle `{}`
  * `-execdir [command] {} +` esegue `command` per ogni cartella trovata, le parentesi graffe `{}` vengono sostituite con il nome di tutte le cartelle trovate. Potrebbe servire fare l'escaping di `;` o delle `{}`
* `chmod [nnn] [p]` cambia i permessi del file `p`\
  i tre numeri `nnn` rappresentano rispettivamente i permessi per l'utente proprietario (`owner`) di `p`, il gruppo a cui appartiene `p`, e tutti gli altri utenti\
  ogni `n` rappresenta una bitmask di 3b `rwx` convertita in numero decimale, ad esempio, per assegnare `rwx r-x --x` (111 101 001) scriveremmo `chmod 751 [p]`
  * `-R` [recursive] applica ricorsivamente sulla cartella `p`
* `chown [user]` assegna `user` come utente di `p`
  * `:(group) [p]` assegna `group` come gruppo di `p`
  * `-R` [recursive] applica ricorsivamente
* `cp [p1a...] [p2]` copia il file\i file `[p1a...]` come file\nella cartella `p2`
  * `-r` [recursive] copia ricorsivamente la cartella `p1`
  * `-l` [link] copia il\i file `p1` come hardlink
  * `-s` [symbolic-link] copia il\i file  `p1` come symlink
* `mv [p1a...] [p2]` sposta il file\i file `[p1a...]` come file\nella cartella `p2`
* `ln [p1a...] [p2]` crea un hardlink nella cartella `p2` per ogni file `p1`\ crea un hardlink `p2` che punta al file `p1`
  * `-s` [symbolic] crea un symlink nella cartella `p2` per ogni elemento `p1`\ crea un symlink `p2` che punta all'elemento `p1`
    * `-r` [relative] crea symlink relativi alla cartella `p2`
* `echo [str]` stampa la stringa `str`
* `cat {p1...}` stampa uno o più file, per reindirizzare l'output su file si può usare `cat [p1a...] > [p2]`
* `uniq {p}` elimina le ripetizioni adiacenti delle righe in `p`
  * `-c` [count] prefissa ogni riga col numero di occorrenze
  * `-d` [repeated] stampa solo le righe duplicate
  * `-D` stampa tutte le righe duplicate
  * `-u` [unique] stampa solo le righe uniche
  * `-i` [ignore-case] il confronto tra le righe è case insensitive
* `tail {p}` stampa le ultime 10 righe di `p`
  * `-n [k]` stampa le ultime `k` righe di `p`
  * `-n +[k]` stampa a partire dalla riga `k` inclusa
* `head {p}` stampa le prime 10 righe di `p`
  * `-n [k]` stampa le prime `k` righe di `p`
  * `-n -[k]` stampa a salire dalla riga `k` inclusa
* `more {p}` stampa una pagina di `p` e poi una riga per volta
  * `-[n]` usa `n` come dimensione della pagina
  * `+[n]` mostra a partire dalla linea `n` inclusa
* `cut {str}` taglia e stampa una porzione della stringa `str` - `cut lorem&ipsum&dolor -d'&' -f2` -> `ipsum`
  * `-f[n1a-n1b],..` (field) stampa solo i campi da `na` a `nb`
    * `-d"[s]"` (delimiter) usa `s` come stringa separatrice, di default è `\t`
* `paste {p...}` affianca riga per riga i `p` separati da `TAB`
  * `-d "str"` usa a rotazione i caratteri di `str` come separatori
* `wc {p}` stampa il numero di righe, parole, e caratteri in `p`
  * `-c` (count) mostra numero di byte
  * `-l` (lines) numero di righe
  * `-w` (words) mostra solo numero di parole
  * `-m` numero di caratteri
  * `-L` (length) lunghezza della riga più lunga
* `grep [regex] {pa...}` cerca le occorrenze in `p` in base a `regex`
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
* `sort {filea...}` ordina il contenuto dei `file` concatenati
  * `-t [s]` usa `s` come separatore di campo
  * `-k [n1],[n2]n\d` usa i campi da `n1` a `n2` come chiavi di ordinamento numerico\alfabetico
  * `-b` [ignore-leading-blanks] ignora eventuali spazi iniziali e finali presenti nelle chiavi di ordinamento
  * `-f` [ignore-case] abilita la case insensitiveness
  * `-n` [numeric-sort] considera numeriche le chiavi di ordinamento di tipo non specificato
  * `-r` [reverse] ordina in modo decrescente
  * `-o [file]` [output=] stampa l'output su `file` ed eventualmente i successivi (fino a fine linea) in caso di 'pareggio'
* `tr [set1] (set2)` sostituisce le occorrenze dei caratteri in `set1`, con i corrispondenti in `set2` - `tr A-Z a-z`, sostituisce le lettere maiuscole con quelle minuscole
  * `-s` (squeeze) sostituisce ogni sequenza di un carattere in `set1` ripetuto, con una singola occorrenza di quel carattere - `tr -s ' '` elimina tutti gli spazi multipli consecutivi
* `sed "[action1;action2;...]" {p1...}` esegue le `action`s consecutivamente per ogni riga di ogni `p`
  * sintassi di `action`:
    * `s/[regex]/[str]/` sostituisce la prima occorrenza di `regex`  con `str`
      * `g` (global) sostituisce tutte le occorrenze in ogni riga
    * `[n\/regex/]/[command]` esegue `command` sulla riga `n`\sulle righe che contengono `regex`
      * sintassi di `command`
        * `d` (delete) elimina
        * `q` (quit) chiude il programma
        * `p` (print) stampa
  * `-i(suffix)` (in-place) modifica direttamente `p` o una copia con suffisso `suffix`
  * `-s` (separate) elabora i file separatamente
  * `-f [p2]` (file-script) prende le `action`s da `p2`
* `date` stampa la data
* `cal` stampa il calendario del mese
  * `-y` (year) stampa il calendario dell'anno corrente
  * `[n]` stampa il calendario dell'anno `n`
  * `-m [month]` stampa il calendario di `month`
* `history` stampa gli ultimi 500 comandi
  * `[n]` stampa gli ultimi `n` comandi
  * sintassi per l'espansione della history: `[command]:(filter)`
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
* `kill [pid\%n]` invia segnale 15 al processo con quel `pid`\all'`n`esimo job in esecuzione sulla shell (se è sospeso raccoglie il segnale quando torna in esecuzione)
  * `-[signal]` invia il segnale `signal`:
    * `9` termina forzatamente
    * `15` chiede la terminazione
* `jobs` elenca i processi in background sulla shell corrente
* `who` stampa gli utenti loggati nel sistema
  * `-a` [all] stampa anche gli utenti dei processi di sistema
* `ps` stampa la lista dei processi generati dalla shell
  * `-u [userlist]` [user] mostra solo i processi degli utenti in `userlist`
  * `-e\A` (everything) mostra tutti i processi
  * `-f` mostra più informazioni, ad esempio lo user ID (UID)
  * `-l` (long format) mostra ancora più informazioni
* `top` visualizza le informazioni di riepilogo del sistema così come un elenco di processi o thread
  * `-b` (batch mode) esegue in modalità batch, utile per ridirezionare l'output
  * `-n [n]` stampa `n` riepiloghi
  * `-d [n]` aggiorna ogni `n` secondi