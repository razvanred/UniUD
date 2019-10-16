# Lezione 3

## Bash: history list

L'history list è un tool fornito dalla shell bash che consente di evitare all'utente di digitare più volte gli stessi comandi:

* bash memorizza nell'history list gli ultimi **500** comandi inseriti dall'utente;
* l'history list viene memorizzata nel file .bash_history nell'home directory dell'utente al momento del logout (e riletta al momento del login)
* il comando ```history``` consente di visualizzare la lista dei comandi:

```bash
$ history | tail --lines=5
489  gcc -o copy_file copy_file.c
490  ./copy_file f1 f2
491  man ls
492  who
493  who | wc -l
```

* ogni riga prodotta dal comando history è detta **evento** ed è preceduta dal **numero dell'evento**

Conoscendo il numero dell'evento corrispondente al comando che vogliamo ripetere, possiamo eseguirlo usando il metacarattere ```!```:

```bash
$!493
$ who | wc -l
1
```

Se l'evento che vogliamo ripetere è l'ultimo della lista è sufficiente usare ```!!```.

Oltre a riferirsi agli eventi mediante i loro numeri, è possibile eseguire ricerche testuali per individurare quello a cui siamo interessati:

```bash
$!who
$ who | wc -l | cat # viene visualizzato solo l'ultimo comando eseguito che corrisponde al pattern
1
```

Se si vuole ripetere un comando eseguito precedentemente operando qualche modifica:

```bash
$!ls:s/al/i/ #!ls:s(ubstitute)/al/(with)i/
$ ls -i
10977524091735467 dircolors.256dark
17451448556251051 lsd_0.16.0_amd64.deb
```

## Bash: command line editing

Comandi di editing per facilitare la ripetizione degli eventi:

* la **freccia su** scorre la history a partire dal più recente a ritroso (**freccia giù** per spostarsi verso i più recenti)
* ```CTRL-A``` sposta il cursore all'inizio del comando scritto
* ```CTRL-E``` sposta il cursore alla fine del comando scritto
* Il tasto **TAB** tenta di completare ciò che stiamo digitando al prompt dei comandi

## Alias

### Path

* ```.``` (directory corrente)
* ```..``` (directory madre)
* ```~user``` (cartella home dell'utente user)

### Input/Output

* ```>``` (ridirezione dell'output)
* ```>>``` (ridirezione dell'output, modalità append)
* ```<``` (ridirezione dell'input)
* ```<<delim``` (ridirezione dell'input da riga di comando (here document))

### Wildcards

* ```*``` (stringa di 0 o più caratteri, ad eccezione del punto)
* ```?``` (un singolo carattere, ad eccezione del punto)
* ```[...]``` (un singolo carattere, tra quelli elencati)

  ```bash
  $ ls file[1234].md
  file1.md
  file2.md
  file3.md
  ```

* ```{...}``` (stringhe separate dalla virgola all'interno delle parentesi)

  ```bash
  $ ls file{1,2,\,,3}
  file1.md
  file2.md
  file3.md
  file,.md
  ```

### Sequenze di comandi

* ```;``` (tutti i comandi separati dal punto e virgola vengono eseguiti)

  ```bash
  > pwd;ls;cd
  ```

* ```||``` (esegue il comando se il precedente fallisce)

  ```bash
  $ ls || echo "hello world"
  file,.md  file-.md  file1.md  file17.md  file2.md  file3.md
  ```

* ```&&``` (esegue il comando se il precedente termina con successo)

  ```bash
  $ ls || echo "hello world"
  file,.md  file-.md  file1.md  file17.md  file2.md  file3.md
  hello world
  ```

* ```(...)``` (raggruppamento di comandi)

  ```bash
  (date;ls;pwd)>out.txt
  ```

### Altro

* ```#``` (introduce un commento)

  ```bash
  ls -l # viasualizzazione dettagliata dei file nella cartella attuale
  ```

* ```|``` (Pipe)

  ```bash
  > ls | more
  ```

* ```\``` (escape, per non far interpretare alla shell i caratteri speciali)

* ```!``` (ripetizione di comandi memorizzati nell'history list)

Il comando ```alias``` serve per creare nuovi alias:

```bash
> alias dir='ls -a'
>
> dir
.  ..  .abc  .file  file1.md  file2.md  file3.md
> alias ls='ls -l'
> ls
total 0
-rw-r--r-- 1 red red 0 Oct 12 13:02 file1.md
-rw-r--r-- 1 red red 0 Oct 12 13:02 file2.md
-rw-r--r-- 1 red red 0 Oct 12 13:02 file3.md
```

### Rimorzione degli alias

Per rimuovere uno o più alias:

```bash
> unalias dir ls
```

All'uscita dalla shell gli alias creati con il comando ```alias``` sono automaticamente rimossi.

## Processi

Ogni processo del sistema ha un **P**rocess **I**dentity **N**umber. Ogni processo può generare nuovi processi (figli). La radice della gerarchia di processi è il processo _init_ con PID=1, il primo processo che parte al boot del sistema.

Il comando ```ps``` fornisce i processi presenti nel sistema.

```bash
$ ps # fornisce i processi dell'utente associati al terminale corrente
PID TTY          TIME CMD                                           803 tty1     00:00:00 zsh                                           844 tty1     00:00:00 ps
```

Legenda:

* ```PID```: PID
* ```TTY```: terminale (virtuale)
* ```TIME```: tempo di CPU utlizzato
* ```CMD```: comando che ha generato il processo
* ```UID```: User Identifier
* ```PPID```: Parent PID
* ```C```: informazione obsoleta sullo scheduling
* ```STIME```: ora di inizio del processo

Il comando ```tty``` permette di ottenere il nome del terminale corrente.

Flags:

* ```-a```: mostra tutti i processi nel sistema associati ad un terminale
* ```-f```: full listing, mostra tutte le colonne disponibili per ciascun processo
* ```-e```: mostra tutti i processi nel sistema, anche non associati ad un terminale
* ```-l```: long listing

### Terminazione di un processo

Per arrestare un processo in esecuzione si può utilizzare:

* ```CTRL-C``` dal terminale stesso su cui il processo è in esecuzione
* ```kill PID``` (da qualsiasi terminale)
* ```KILL -s SIGKILL PID``` (tramite il segnale ```SIGKILL```)

### Kill

```kill``` è un comando che permette di inviare un determinato segnale a uno o più processi (o ad un gruppo di processi).

Per maggiori informazioni sulla differenza tra i segnali ```SIGTERM``` e ```SIGKILL``` visita il [sito](https://major.io/2010/03/18/sigterm-vs-sigkill/).

### Processi in Background

Un comando seguito da ```&``` dà luogo ad uno o più **processi in background**. I processi in background sono eseguiti in una **sottoshell**, in parallelo al processo padre (la shell) e **non** sono controllati da tastiera.

```bash
$ gedit hello
[1] 5642 # [numero del job] (PID del processo)
```

### Controllo di Job

Il comando ```jobs``` mostra la lista dei job in esecuzione.

```bash
$ jobs
[1]    running    gedit
[2]    suspended (signal)  nano
[3]  - suspended (signal)  nano
[4]  + suspended (signal)  nano
```

Un job si può **sospendere** e poi **rimandare in esezione**.

```bash
$ gedit # job in foreground
# Tramite CTRL-Z sospendo il job, ritornando il focus alla shell
[1]  + 7605 suspended  gedit
$ fg # riprende l'esecuzione del job in foreground
[1]    7605 continued  gedit
# CTRL-Z
$ bg # riprende l'esecuzione del job in background
[1]    7605 continued  gedit
$ kill %1 # Termina il job 1
[1]    7883 terminated  gedit
```

Nota bene: se il processo si trova nello stato ```suspended```, non è possibile terminarlo invandogli solamente il segnale ```SIGTERM```. Ci sono tre opzioni:

1. (brutale) Invia un segnale SIGKILL: ```kill -9 %(numero del job)```
2. Riprendi l'esecuzione del processo in background e poi terminalo inviandogli un segnale ```SIGTERM``` (seguendo l'esempio di sopra).
3. Se il segnale ```SIGTERM``` è già stato inviato mentre il processo era in stato ```suspended```, basta riprenderlo in background o in foreground: non appena si riprende riceve il segnale e lo gestisce nella maniera prevista.

## Monitoraggio della memoria

Il comando ```top``` fornisce informazioni sulla memoria utilizzata dai processi, aggiornate ad intervalli di qualche secondo. I processi sono elencati secondo la quantità di tempo di CPU utilizzata.

La prima riga indica il carico del sistema nell'ultimo minuto, negli ultimi 5 minuti e negli ultimi 15 minuti rispettivamente. Il carico è espresso come numero di processori necessari per far girare tutti i processi a velocità massima.

La seconda contiene il numero dei processi nel sistema e il numero per ciascuno stato.

La terza contiene l'utilizzo della CPU.

La quarta contiene finromazioni sulla memoria.

## Esercizi

* Ridefinire il comando ```rm``` in modo tale che non ci sia chiesta conferma prima della cancellazione dei file.

  ```bash
  alias rm="rm -f"
  ```

* Definire il comando ```rmi``` (```rm``` interattivo) che chiede conferma prima di rimuovere un file

  ```bash
  alias rmi="rm -i"
  ```

* Sapendo che il comando ```ps``` serve ad elencare i processi del sistema, scrivere una pipeline che fornisca in output il numero di tutti i processi in esecuzione.

  ```bash
  expr $(ps -e | wc -l) - 1
  ```

* Salvare in un file di testo l'output dell'ultimo evento contente il comando ```ls```

  ```bash
  echo !ls >file # non funziona se eseguito da file sh, in quanto la bash interpreta al volo il carattere !
  ```

* Scrivere un comando che fornisce il numero dei comandi contenuti nella history list

  ```bash
  history | wc -l
  ```

* Scrivere un comando che fornisce i primi 15 comandi della history list

  ```bash
  history | head --lines=15
  ```

* Quali sono i comandi Unix disponibili nel sistema che iniziano con ```lo```?

  Basta scrivere su terminale ```lo``` seguito dal tasto TAB e si otterrà la lista dei comandi.

* Fornire almeno due modi diversi per ottenere la lista dei file della vostra home directory il cun nome inizia con al.

  ```bash
  # Primo metodo
  ls $HOME/al*
  # Secondo metodo
  cd $HOME && ls al*
  ```

* Qual'è l'effetto dei seguenti comandi?

  ```bash
  ls -R || echo file non accessibili > tmp
  # ls -R visualizza in modo ricorsivo il contenuto della cartella attuale (se non riesce ad accedere ad una delle cartelle interne viene scritto sul file tmp il messaggio file non accessibili)
  ```

  ```bash
  (who | grep rossi) && cd ~rossi
  # who ritorna gli utenti connessi attualmente al sistema, l'output viene passato a grep, il quale cerca se vi è presente un utente rossi. Se la pipeline ha successo, la directory attuale viene cambiata nella home di rossi.
  ```

  ```bash
  (cd / ; pwd ; ls | wc -l)
  # tutti i comandi qui vengono eseguiti sequenzialmente anche se uno di loro non viene completato con successo
  # all'inizio la directory attuale viene cambiata nella directory root, dopodiché viene stampato su stdout la directory corrente (/) e infine viene stampato il numero di file e cartelle (visibili) presenti all'interno della cartella attuale
  ```

* Qual'è la differenza tra programma e processo?

  Il programma è il file o l'insieme di file in cui il programmatore scrive le istruzioni che deve eseguire l'elaboratore per completare un incarico.

  Il processo invece è l'istanza/astrazione del programma in esecuzione, compresi i valori attuali del contatore di programma, dei registri e delle variabili. Concettualmente ogni processo dispone di una CPU virtuale, ma in realtà la CPU passa avanti e indietro da processo a processo (**multiprogrammazione**).

* Qual'è la differenza tra processo e job?

  Un processo in Unix, che esegue un determinato job, può essere eseguito in _foreground_ oppure in _background_.
  
  Un job in foreground viene visualizzato sulla shell e risulta impossibile comunicare con la shell fin quando il processo non viene terminato / sospeso / interrotto.

  ```bash
  # per far partire un job in foreground
  > gedit
  # se il job gira in background oppure si trova in stato sospeso
  > fg %1
  ```

  Un job in background parte e ritorna il focus alla shell, quindi l'utente può continuare ad interagire con essa anche se il processo è in esecuzione. Un job in background può scrivere sullo stdin del terminale corrente.

  ```bash
  # per far partire un job in background
  > gedit &
  [1] 2432
  # se il job si trova in stato sospeso, per riprenderlo in background
  > bg %1
  ```

  Fonte: [WikiBooks](https://en.wikibooks.org/wiki/A_Quick_Introduction_to_Unix/Job_Control)

* Scrivere una pipeline che fornisca in output il numero di processi appartenenti all'utente root.

  ```bash
  expr $(ps -u root | wc -l) - 1
  ```

* Come si può mandare un processo in esecuzione da foreground a background in modo da rendere il terminale nuovamente disponibile per l'invio di ulteriori comandi?

  ```bash
  # CTRL-Z per mandare il processo in stato suspended
  # La shell ci ritornerà il numero del job e il PID del processo
  # Supponiamo che il job da prendere in considerazione sia pari a 1
  bg %1 # numero del job
  ```
