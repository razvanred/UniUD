# Lezione 5

## Comando filtro: ```sed```

**S**tream **ED**itor permette di modificare il testo passato da un comando ad un altro in una pipeline. Utile dato che tale testo non risiede in memoria persistente, dunque non modificabile con un editor di testo tradizionale. ```sed``` tuttavia puà prendere in input dei file:

```bash
sed actions files
```

* Se non si specificano azioni ```sed``` stampa su stdout le linee in input, lasciandole inalterate
* Se vi è più di un'azione, esse possono essere specificate sulla riga di comando precedendo ognuna con l'opzione ```-e```, oppure possono essere lette da un file esterno specificato sulla linea di comando con l'opzione ```-f```.
* Se non viene specificato un indirizzo o un intervalli di indirizzi di linea su cui eseguire l'azione, quest'ultima viene applicata a tutte le linee in input. Gli indirizzi di linea si possono specificare come numeri o **espressioni regolari**.

### Esempi d'uso dol comando ```sed```

```zsh
% sed '4,$d' /etc/passwd
# stampa a video soltanto le prime tre righe del file /etc/passwd.
# $ sta per ultima riga
# d sta per delete
# il numero 4 rappresenta la quarta riga da cui si andrà ad eliminare il contenuto fino all'ultima riga
```

```zsh
% sed 3q /etc/passwd
# stesso effetto del precedente comando: in questo caso sed termina la sua lettura non appena finisce di leggere la terza riga (q - quit dopo la linea 3)
```

```zsh
% sed /sh/y/:0/_%/ /etc/passwd
# sostituisce in tutte le righe che contengono la stringa sh il carattere _ con il carattere _ ed il carattere 0 con il carattere %
```

```zsh
% sed '/sh/!y/:0/_%/' /etc/passwd
# effettua la stessa sostituzione di caratteri come sull'esempio precendente
# ma stavolta sulle righe che non contengono sh
# quoting per evitare l'interpretazione di ! come metacarattere da parte della shell
```

### Sostituizione del testo con ```sed```

```s/expr/new/flags```

* ```expr``` è l'espressione da cercare
* ```new``` è la stringa da sostituire al posto di ```expr```
* ```flags```:
  * _num_: un numero da 1 a 9 che specifica quale occorrenza di ```expr``` deve essere sostituita (di default è la prima)
  * ```g```: ogni occorrenza di ```expr``` viene sostituita
  * ```p```: la linea corrente viene stampata sullo stdout nel caso vi sia stata una sostituzione
  * ```w file```: la linea corrente viene accodata nel file _file_ nel caso vi sia stata una sostituzione.

#### Esempi di sostituzione con ```sed```

* ```sed '/^root/,/^bin/s/:x:/::/w disabled.txt' /etc/passwd``` sostituisce la password criptata rappresentata da ```x``` con la stringa vuota nelle righe in input comprese fra quella che inizia con ```root``` e quella che inizia con ```bin```; tali righe sono poi accodate nel fila ```disabled.txt```.
* ```cat /etc/passwd | sed 's?/bin/.*sh$?/usr/local&?'```
  * cerca tutte le righe in input in cui compare la stringa corrispondente a cui compare l'espressione regolare ```/bin/.*sh$```(come ```/bin/bash```) e sostituisce quest'ultima con la stringa corrispondente a ```/usr/local/bin/.*sh$```;
  * siccome il carattere separatore di ```sed``` compare nella stringa da cercare si è usato il carattere ```?``` come separatore;
  * il carattere ```&``` viene rimpiazzato automaticamente da ```sed``` con la stringa cercata (corrispondente a ```/bin/.*sh$```).

## Shell Script

* Programmi interpretati dalla shell scritti in un linguaggi i cui costrutti atomici sono i comandi Unix.
* Comandi combinabili in sequenza o mediante i costrutti usuali di un linguaggio di programmazione.
* Faremo riferimento alla shell ```bash```.
* Per poter essere eseguito uno script deve prima essere reso **eseguibile**.

### Esempio script

```bash
# dirsize
#!/bin/bash

ls /usr/bin | wc -w
```

Console:

```bash
% chmod 700 dirsize # dirsize viene reso eseguibile per l'owner
% ./dirsize # invocazione script
   1011 # risultato script
```

### Esecuzione dello script

Lo script viene eseguito in una **sottoshell** della shell corrente.

Il comando ```set -v```/```set -x``` fa si che durante l'esecuzione di uno script la shell visualizzi i comandi nel momento in cui li legge/esegue (```set -``` annulla l'effetto di ```set -v```/```set -x```). Utile per il debugging.

* Esempio 1:

  ```bash
  # data
  #!/bin/bash

  set -x
  echo the date today is:
  date
  ```

  Console:

  ```zsh
  % chmod u+x data
  % ./data
  + echo The date today is:
  The date today is:
  + date
  Wed Oct 23 19:25:01 CEST 2019
  ```

* Esempio 2:
  
  ```bash
  # sost
  #!/bin/bash

  set -v
  cd TEXT
  ls *.txt
  sed s/\#/\;\;\;/ file.txt
  ```

  Console:

  ```zsh
  % chmod u+x sost
  % ./sost
  cd TEMP
  ls *.txt
  file.txt
  sed s/\#/\;\;\;/ file.txt
  ;;; questo è un programma
  ;;; di prova
  ```

### Variabili

Le **variabili** della shell sono:

* Stringhe di caratteri a cui è associato uno **spazio in memoria**;
* anche il valore è una **stringa di caratteri** e si accede mediante il simbolo ```$```;
* possono essere usate sia sulla linea di comando che negli script;
* non si dichiarano esplicitamente: ```variabile=valore``` (non lasciare spazi bianchi a sinistra e a destra dell'operatore ```=```);
* **locali** alla shello o allo script in cui sono definite.

#### Esempio sull'uso di variabili locali

```zsh
% x=valore
% y='ciao mondo'
% echo il valore di x: $x
il valore di x: valore
% echo il valore di y: $y
il valore di y: ciao mondo
% echo y # senza il $ viene trattato come un carattere (o stringa) normale da stampare
y
```

#### Variabili d'ambiente

Per rendere una variabile locale globale (**variabile di ambiente**) si usa il comando ```export```:

```zsh
% export x # x ora diventa una variabile di ambiente
```

Esiste un insieme di variabili di ambiente speciali riconosciute dalla shell e definite al momento del login:

* **PS1**: prompt della shell
* **PS2**: secondo prompt della shell; utilizzato per esempio in caso di ridirezione dell'input dalla linea di comando
* **PWD**: pathname assoluto della directory corrente
* **UID**: user identifier corrente
* **PATH**: lista di pathname di directory in cui la shell cerca i comandi
* **HOME**: pathname assoluto della home directory

##### Esempio d'uso delle variabili di ambiente

```zsh
% PATH="$PATH:$HOME/development/flutter/bin" # aggiungiamo alla variabile path una nuova directory in cui cercare
% PS1="command> " # modifica del prompt della shell
command> PS2="heredoc> " # modifica del secondo prompt della shell (heredoc)
command> cat <<term
heredoc> hello world
heredoc> how are you
heredoc> term
```

### Parametri

Le variabili $1, $2, ..., $9 sono variabili speciali associate al primo, al secondo, ..., nono parametro passato sulla linea di comando quando viene invocato uno script.

```bash
#!/bin/bash

mkdir $1
mv $2 $1/$2
```

Console:

```zsh
% chmod u+x cut.sh
% ./cut.sh folder file.txt
```

Se uno script ha più di 9 parametri, si utilizza il comando ```shift``` per fare lo shift a sinistra dei parametri e poter accedere ai parametri oltre al nono:

```zsh
#!/bin/bash
shift
echo decimo parametro: $9
```

### Variabili di stato automatiche

* Variabili speciali che servono a gestire lo **stato** e sono aggiornate automaticamente dalla shell;
* Accessibili solo in modalità **lettura**;
* Per esempio, al termine di esecuzione di ogni comando UNIX, viene restituito un valore di uscita, **exit status**, uguale a 0 se l'esecuzione è terminata con successo: la variabile speciale ```$?``` contiene il valore di uscita dell'ultimo comando eseguito.

```zsh
% cd
% echo $?
0
% ./cut.sh file folder
mkdir: file: File exists
mv: rename folder to file/folder: Not a directory
% echo $?
1
```

Il comando ```exit n```, dove n è un numero, usato all'interno di uno script, serve per terminare l'esecuzione e assegnare alla variabile di stato il valore  ```n```.

```bash
#!/bin/bash

mkdir $1
mv $2 $1/$2

exit 0 # anche se i comandi precedenti termineranno con un codice di errore, lo script terminerà in questo caso SEMPRE con codice 0
```

#### Lista variabili di stato automatiche

| Variabile          | Contenuto                                                             |
|--------------------|-----------------------------------------------------------------------|
| ```$?```           | exit status dell'utlimo comando eseguito dalla shell                  |
| ```$$```           | PID della shell corrente                                              |
| ```$!```           | il PID dell'ultimo comando eseguito in background                     |
| ```$-```           | le opzioni della shell corrente                                       |
| ```$#```           | numero di parametri forniti allo script dalla linea di comando        |
| ```$*```, ```$@``` | lista di tutti i parametri passati allo script dalla linea di comando |

In particolare, ```$$``` viene usata per generare file temporanei che siano univoci tra utenti diversi e istanze di shell diverse.

### Login script

Script speciale che viene eseguito automaticamente al momento del login. In alcune versioni di UNIX/Linux, tale script è contenuto in uno dei file ```.bash_profile```, ```bash_login```, ```.bashrc```, ```.profile```, memorizzati nella home directory degli utenti.

Il login script contiene alcuni comandi che è utile eseguire al momento del login, come la definizione di alcune variabili di ambiente.

Esiste anche un login script _globale_ contenuto nel file ```etc/profile``` in cui l'amministratore di sistema può memorizzare dei comandi di configurazione che valgano per tutti. Questo script è eseguito prima che venga eseguito quello dei singoli utenti.

Lo script di logout eseguito al momento dell'uscita dalla shell, si chiama solitamente ```.bash_logout```.

## Esercizi

* Creare una sottodirectory ```bin``` all'interno della proprima home directory in cui mettere gli script. Fare in modo che gli script contenuti in ```bin``` possano essere invocati da qualunque directory con il nome del file, senza dover specificare l'intero pathname.

  ```zsh
  % mkdir bin && cd bin
  # creazione file di script
  % export PATH=$PATH:$HOME/bin
  ```

* Qual'è l'effetto della seguente sequenza di comandi? Perché?

  ```zsh
  % cat >chdir
  cd ..
  Ctrl-D
  % chmod 700 chdir
  % chdir
  % pwd
  ```

  All'interno della present working directory viene creato il file script ```chdir```; l'user poi dà il permesso di lettura, scrittura ed eseguzione a sé stesso sul file, e toglie tutti i permessi agli utenti group e others; ora l'utente è in grado di eseguire il comando, e lo esegue. L'ultimo comando ritornerà la parent directory della pwd precedente.

* Creare un alias permanente ```lo``` per il comando ```exit```

  ```zsh
  % cd $HOME && echo alias lo='exit' >>.bashrc
  # basta un riavvio del terminale
  ```

* Progettare uno script che prende come parametro una stringa ed un file di testo e controlla se la stringa compare nel file.

  ```bash
  #!/bin/bash
  if (($# != 2))
  then
    echo "Usage: script.sh [string] [file]"
    exit 1
  
  fgrep $1 $2
  ```

* Il comando ```read``` assegna alla variabile speciale ```REPLY``` un testo acquisito da stdin. Qual'è l'effetto dello script ```words``` contenente i seguenti comandi?

  ```bash
  #!/bin/bash
  echo -n "Enter some text: "
  read one two restofline
  echo "The first word was: $one"
  echo "The second word was: $two"
  echo "The rest of line was: $restofline"
  exit 0
  ```

  ```zsh
  % ./words
  Enter some text: first second hello world
  The first word was: first
  The second word was: second
  The rest of line was: hello world
  ```

* Qual'è l'effetto della seguente sequenza di comandi? Perché?

  ```bash
  $ cat >data
  echo -n the date today is:
  date
  Ctrl-D
  > chmod 700 data
  > data
  ```

  Viene creato un file data nella present working directory; l'utente poi dà i permessi di lettura, scrittura ed esecuzione sul file a sé stesso, e toglie tutti i permessi al group e agli others; infine, esegue lo script che mostrerà la data del giorno presente.

* Scrivere uno script che estragga soltanto i commenti dal file con estensione ```java``` fornito come primo argomento, sostituendo ```//``` con la stringa ```linea`di commento del file <nome_del_file>:```. Inoltre i commenti estratti devono essere salvati nel file fornito come secondo argomento.

  ```bash
  #!/bin/bash

  if (($# != 2))
  then
    echo "Usage: script.sh [file(.java)] [newfile]"
    exit 1
  fi

  javaFile=$(echo $1 | grep .java$)

  if [ -z $javaFile ]
    echo "No Java file given in input"
    exit 2
  fi

  sed "s?//?linea di commento del file <$javaFile>:?w $2" -n $1
  ```
