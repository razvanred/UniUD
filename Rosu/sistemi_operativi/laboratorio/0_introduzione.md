# Corso di Laboratorio di Sistemi Operativi

- Orario di ricevimento:
  - Venerdì 15.00-16.00, oppure previo appuntamento
  - Email: [Ivan Scagnietto](ivan.scagnetto@uniud.it)
  - Media pesata dei voti di teoria e laboratorio: ```(9*voto_teoria + 3*voto_lab) / 12```

## La shell di UNIX e GNU/Linux

- La parte del sistema operativo UNIX dedita alla gestione dell'interazione con l'utente è la **shell**, ovvero l'**interfaccia a carattere**.
  - L'utente impartisce i comandi al sistema digitandoli ad un apposito **prompt**
  - Il sistema stampa su schermo del terminali evenutali messaggi all'utente in seguito all'esecuzione dei comandi, facendo poi riapparire il prompt, in modo da continuare l'interazione.
- Versioni moderne di UNIX forniscono **X-Windows**, un gestore grafico (per interfacce a finestra), che consente di inviare comandi tramite menu, utilizzando un mouse.
- **X-Term** è un emulatore di terminale che gira sotto X-Windows, fornendo localmente un'interfaccia a carattere.
- Gestori di ambienti desktop (che girano su X-Windows) come KDE, Gnome, ecc. forniscono altre versioni di emulatori di terminali più avanzati

### Tipi di Shell

- **sh** Bourne shell
- **bash** Bourne again shell
- **csh** C shell
- **tcsh** Teach C shell
- **ksh** Korn shell

Quando viene invocata una shell, automaticamente al login o esplicitamente:

1. Viene letto un file speciale nella home directory dello user, contenente informazioni per l'inizializzazione;
2. Viene visualizzato un **prompt**, in attesa che l'utente invii un comando;
3. Se l'utente invia un comando, la shell lo esegue e ritorna al punto 2; ad esempio, ```echo $SHELL``` stampa sullo schermo del terminale il percorso della shell di login, mentre il comando ```bash``` invoca la shell bash.

Per terminare la shell si possono utilizzare i seguenti comandi:

- Premere Ctrl-D (quando la linea di comando è vuota)
- Digitare i comandi ```logout``` o ```exit```

## File in UNIX

- Ordinari
- Directory
- Speciali (link)

I file sono organizzati in una struttura gerarchica ad albero.

## Il pathname

Ci si riferisce ai file con il **pathname** che può essere:

- assoluto (rispetto a root ```/```): ```/home/bianchi/progetto/a```
- relativo (rispetto alla directory corrente): ```progetto/a```.

(in questo momomento mi trovo nella directory ```bianchi```).

- Present working directory:

  ```bash
  > pwd
  /home/bianchi
  ```

- Change directory:

  ```bash
  cd /bin
  ```

  ```cd``` senza argomenti sposta l'utente nella sua home directory. Per spostarsi nella directory "madre" (o superiore) basta eseguire:

  ```bash
  cd ..
  ```

  dove ```..``` è l'alias per la directory superiore.

Esempio:

```bash
> pwd
/home/bianchi
> cd ./progetto # dove . è l'alias per la directory corrente
> pwd
/home/bianchi/progetto
```

## Comandi per manipolare file e directory

- Listing dei file:

```bash
> ls
> ls -l # visualizzazione dettagliata dei file e delle cartelle presenti
> ls -a # possibilità di vedere anche i file nascosti (con il . davanti)
> ls -al # alternative: ls -la, ls -a -l, ls -l -a (visualizzazioe dettagliata di tutti i file e delle cartelle presenti, anche nascosti)
> ls -l /bin
```

- Creazione/rimorzione delle directory:

```bash
> mkdir d1
> rmdir d1
```

- Copia il file f1 in f2:

```bash
> cp f1 f2
```

- Sposta/rinomina il file f1 in f2:

```bash
> mv f1 f2
```

- ```cp``` ed ```mv``` come primo argomento possono prendere una lista di file; in tal caso, il secondo argomento deve essere una directory:

```bash
> cp f1 f2 f3 d1 # copia i file f1, f2 ed f3 nella directory d1
```

## Esempio uso comando ls

Eseguendo il comando ```ls -l /bin``` si ottiene il seguente output:

```bash
...
lrwxrwxrwx   1 root   root        4 Dec  5 2000 awk -> gawk
-rwxr-xr-x   1 root   root     5780 Jul 13 2000 basename
-rwxr-xr-x   1 root   root   512540 Aug 22 2000 bash
...
```

Da sinistra verso destra abbiamo:

1. tipo di file (```-``` file normale, ```d``` directory, ```l``` link, ```b``` block device, ```c``` character device)
2. permessi
3. numero di hard link al file
4. nome del proprietario del file
5. nome dell'insieme di utenti che possono accedere al file come gruppo
6. grandezza del file in byte
7. data di ultima modifica
8. nome del file

## I permessi dei file

Linux è un sistema **multiutente**. Per ogni file ci sono 4 categorie di utenti:

- root - amministratore del sistema - ha tutti i permessi (lettura, scrittura, esecuzione) su tutti i file
- owner (oppure **user**)
- group
- world (oppure **others**)

L'accesso ai file delle ultime 3 categorie deve essere regolato dai permessi.

```bash
> ls -l /etc/passwd
-rw-r--r--   1 root   root   981 Sep 20 16:32 /etc/passwd
```

Il blocco di caratteri ```-rw-r--r--``` rappresenta i permessi di accesso al file. I primi 3 (```rw-```) sono riferiti all'owner. Il secondo blocco di 3 caratteri (```r--```) è riferito al group e l'ultimo blocco (```r--```) è riferito alla categoria world.

La prima posizione di ogni blocco rappresenta il permesso di **lettura** (```r```), la seconda il permesso di **scrittura** (```w```) e la terza il permesso di **esecuzione** (```x```). Un trattino (```-```) in una qualsiasi posizione indica l'assenza del permesso corrispondente.

N.B.: per accedere ad una directory, bisognerà avere il permesso di esecuzione su di essa.

## Il comando chmod

L'owner di un file può cambiarne i permessi tramite il comando ```chmod```:

- ```chmod 744 f1``` (imposta i permessi del file f1 a rwxr--r--)
  Infatti: corrisponde ad un insieme di 3 gruppetti di numeri scritti in ottale (111 100 100).
- ```chmod u=rwx,go=r f1``` (produce lo stesso effetto del comando precedente)
  - u rappresenta l'owner
  - go rappresenta il group e gli altri (world)
- Inoltre:
  - ```+``` aggiunge i permessi che lo seguono
  - ```-``` toglie i permessi che lo seguono
  - ```=``` imposta esattamente i permessi che lo seguono
  Quindi l'effetto di ```chmod g+r f1``` è in generale diverso da ```chmod g=r f1```.

## Ulteriori comandi

- Visualizzazione del contenuto di un file:

```bash
> cat f1 # visualizza il contenuto del file f1
> more f1
> tail f1
> head f1
```

- Consultazione del manuale online:
  - Sezione 1: comandi
  - Sezione 2: system call
  - Sezione 3: funzioni di libreria
  - Sezione 4: file speciali e driver
  - Sezione 5: formati di file e convenzioni
  - Sezione 6: giochi e screensaver
  - Sezione 7: miscellenea
  - Sezione 8: amministrazione di sistema e demoni

  ```bash
  > man passwd
  > man -a passwd
  > man -s2 mkdir
  > man man
  ```

- Pulizia dello schermo: ```clear```

## Inode e link

In UNIX, ad ogni file corrisponde un numero di **inode**, che è l'indice in un array memorizzato su disco. Ogni elemento dell'array contiene le informazioni relative al file (data di creazione, proprietario, dove si trova il contenuto del file su disco, ...).

Le directory sono tabelle che associano nomi di files a numeri di inode.

Ogni entry di una directory è un **link**.

## Link

- Creazione di **link (hard)**

```bash
ln f2 f2_new # il file f2_new ha lo stesso inode di f2
```

- Creazione di un **link simbolico**

```bash
ln -s g1 g1_new
```

Un link simbolico è un tipo di file speciale in UNIX; ```g1_new``` è un file di testo che contiene il pathname di g1.

## Esercizi

- Qual'è il pathname della vostra home directory?

  Attraverso il comando ```cd && pwd``` il pathname della mia home directory è ```/home/red```.

- Visualizzare i file della home directory ordinati in base alla data di ultima modifica.

  Prima ci si documenta attraverso il comando ```man ls```.
  Comando: ```ls --sort=time```.

- Che differenza c'è tra ```cat```, ```more``` e ```tail```?

  - ```cat``` concatena i file in input e li stampa sullo __standard output__;
  - ```more``` legge per intero un file e mostra su console il contenuto suddiviso per pagine;
  - ```tail``` mostra di default le ultime 10 righe del file (numero modificabile specificando il parametro ```--lines=n```).

- (RISPOSTA DIRETTA) Un link simbolico può puntare ad un altro link che a sua volta punta ad un file, esiste un limite di link simbolici che si possono avere in catena che dipende dal sistema operativo in uso (altrimenti si potrebbe generare una situazione di ciclo infinito: se f1 è un collegamento ad f2 ed f2 è un collegamento ad f1, e il limite di link simbolici del sistema operativo è pari a 40, accedendo ad uno dei due collegamenti il sistema verrà reindirizzato per 40 volte da un file all'altro, finché non viene raggiunto il limite e lancia un messaggio di errore). Ci sono diversi svantaggi dei link simbolici rispetto ai link hard, tra cui:

  - Se il file su cui viene effettuato il collegamento viene cancellato il link simbolico viene invalidato;
  - I link simbolici sono file di testo, dunque a loro volta sono file che corrispondono ad un inode. Si deve prima accedere al contenuto del file contente il percorso del file puntato; solo allora si può accedere al contenuto del file puntato, a cui corrisponde un altro inode diverso. Dati questi accessi in più ne risentono le performance.

- Trovate un modo per ottenenere l'elenco delle subdirectory contenute ricorsivamente nella vostra home.

  ```bash
  cd && ls --recursive # oppure ls -R
  ```

- Trovare due modi diversi per creare un file

  ```bash
  touch file
  cat >file # subito dopo basta premere CTRL-D
  echo >file
  ```

- Che effetti producono questi comandi? Perché?

  ```bash
  cd # torna alla home dell'utente (solitamente /home/utente)
  mkdir d1 # crea una cartella all'interno della cartella home
  chmod 444 d1 # setta i permessi r-- sugli utenti owner, group e others
  cd d1 # l'owner cerca di accedere a /home/utente/d1, ma a causa del comando precedente il permesso risulta negato (mancano i diritti di esecuzione sull'owner)
  ```
