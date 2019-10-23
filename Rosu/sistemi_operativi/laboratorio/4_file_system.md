# Lezione 4

## Il filesystem di Unix/Linux

Comunemente, in un elaboratore l'informazione è memorizzata permanentemente sui **dischi fissi**: ogni disco fisso può essere suddiviso in **partizioni** e ogni partizione può contenere un file system con una propria **top level directory**.

Come permettere agli utenti di accedere ai vari filesystem contenuti nelle diverse partizioni:

* si possono avere **root directory distinte** (come in Windows: ```C:\```, ```D:\``` ecc.), una per ogni partizione. Quindi per riferirsi ad un file, bisogna usare un pathname che parta dalla root directory giusta (es. ```C:\Users\Razvan\Desktop\file.md```).
* Unix/Linux invece fanno in modo che i diversi filesystem vengano combinati in un'**unica struttura gerarchica**, _montando_ la top level directory di una partizione come foglia del filesystem di un'altra partizione.

```/dev/hda1``` e ```/dev/hda2``` sono partizioni differenti. Le informazioni su quali filesystem montare al boot ed in che modo sono contenute nel file ```/etc/fstab```. Il comando per montare i filesystem è ```mount <file speciale> <mount point>``` e, solitamente, solo l'utente ```root``` può utilizzarlo. ```mount```  senza argomenti elenca i filesystem in uso nel sistema.

## Controllo dello spazio su disco

Per controllare la quantità di spazio su disco in uso:

```bash
% df
Filesystem    512-blocks      Used Available Capacity iused      ifree %iused  Mounted on
/dev/disk1s5   261718752  20965360 121155944    15%  481705 1308112055    0%   /
devfs                389       389         0   100%     679          0  100%   /dev
/dev/disk1s1   261718752 113964056 121155944    49%  723350 1307870410    0%   /System/Volumes/Data
/dev/disk1s4   261718752   4196400 121155944     4%       3 1308593757    0%   /private/var/vm
/dev/disk0s3   236893304 133024584 103868720    57%  420701   51992635    1%   /Volumes/OS
map auto_home          0         0         0   100%       0          0  100%   /System/Volumes/Data/home
```

* **Filesystem** indica il device corrispondente (eventualmente virtuale);
* **512-blocks** indica il numero di blocchi totale;
* **Used** indica il numero di blocchi occupati;
* **Available** indica il numero di blocchi liberi;
* **%iused** indica la percentuale in uso;
* **Mounted on** indica il punto di mount.

Per controllare la quantità di spazio su disco utilizzata da una directory (in blocchi):

```bash
% du sistemi_operativi
40  sistemi_operativi/teoria/4_file_system/copy_file/.idea
80  sistemi_operativi/teoria/4_file_system/copy_file
96  sistemi_operativi/teoria/4_file_system
120 sistemi_operativi/teoria
80  sistemi_operativi/laboratorio
216 sistemi_operativi
```

## Comandi su file

* Confronto tra file:

  1. ```cmp file1 file2``` restituisce il primo byte ed il numero di linea in cui ```file1``` e ```file2``` differiscono (se sono uguali, non viene stampato nulla a video)
  2. ```diff file1 file2``` restitiuisce la lista di cambiamenti da apportare a ```file1``` per renderlo uguale a ```file2```.

* Ricerca di file:

  ```bash
  % find <pathnames> <expressions>
  ```

  attraversa ricorsivamente le directory specificate in ```<pathnames>``` applicando le regole specificate in ```<expression>```a tutti i file e sottodirectory trovate. ```<expression>``` può essere:

  1. Un'opzione,
  2. Una condizione,
  3. Un'azione.

  Esempi:

  ```bash
  % find . -name '*.c' -print
  # cerca ricorsivamente a partire dalla directory corrente tutti i file con estensione c e li stampa a video
  ```

  ```bash
  % find . -name '*.bak' -ls -exec rm {} \;
  # cerca ricorsivamente a partire dalla directory corrent tutti i file con estensione bak
  # li stampa a video con i relativi attributi (-ls) e li cancella (-exec rm {} \;)
  # il carattere \ serve per fare il "quote" del ;
  ```

  ```bash
  % find /etc -type d -print
  # cerca ricorsivamente a partire dalla directory /etc tutte e solo le sottodirectory, stampandole a video
  ```

## Comandi di filtro

I filtri sono una particolare classe di comandi che possiedono i seguenti requisiti:

* prendono l'input dallo **stdin**
* effettuano delle operazioni sull'input ricevuto
* inviano il risultato delle operazioni allo **stdout device**

Esempi:

```bash
% uniq file
# restituisce in output il contenuto del file file, sostituendo le linee adiacenti uguali con un'unica linea
```

### ```grep```, ```fgrep```, ```egrep```

I comandi:

* ```grep``` General Regular Expression Parser,
* ```fgrep``` Fixed General Regular Expression Parser,
* ```egrep``` Extended General Regular Expression Parser,

restituiscono solo le linee dell'input fornito che contengono un **pattern** specificato tramite espressione regolare o stringa fissata.

#### Sintassi

```grep [options] pattern [filename]```
```fgrep [options] string [filename]```
```egrep [options] pattern [filename]```

#### Opzioni dei comandi grep

* ```-i``` ignora la distinzione tra lettere minuscole e lettere maiuscole,
* ```-l``` fornisce la lista dei file che contengono il pattern/string,
* ```-n``` le linee in output sono precedute dal numero di linea,
* ```-v``` vengono restituite solo le linee che non contengono il pattern/string,
* ```-w``` vengono restituite solo le linee che non contengono il pattern/string come parola completa,
* ```-x``` vengono restituite solo le linee che coincidono esattamente con il pattern/string.

Consulta la lista dei metacaratteri delle espressioni regolari.

#### Esempi

```bash
% fgrep rossi /etc/passwd
# fornisce in output le linee del file /etc/passwd che contengono la stringa fissata rossi.
```

```bash
% egrep -nv '[agt]+' relazione.txt
# fornisce in output le linee del file relazione.txt che non contengono stringhe composte dai caratteri a, g, t
# ogni linea è preceduta dal suo numero
```

```bash
% grep -w print *.c
# fornisce in output le linee di tutti i file con estensione c che contengono la parola intera print
```

```bash
% ls -al . | grep '^d.......w.'
# fornisce in output le sottodirectory della directory corrent che sono modificabili dagli utenti ordinari (others)
```

```bash
% egrep '[a-c]+z' doc.txt
# fornisce in output le linee del file doc.txt che contengono una stringa che ha un prefisso di lunghezza non nulla, costituito solo da lettere a, b, c, seguito da una z.
```

### ```sort```

Il comando ```sort``` prende in input delle linee di testo, le **ordina** (tenendo conto delle opzioni specificate dall'utente) e le invia in output.

* ```sort``` tratta ogni linea come una collezione di vari campi separati da delimitatori (default: spazi, tab, ecc.)
* l'ordinamento di default avviene in base a tutta la linea (in base a tutti i suoi campi) ed è alfabetico

#### Opzioni del comando sort

* ```-b``` ignora eventuali spazi presenti nelle chiavi di ordinamento,
* ```-f``` ignora le distinzioni fra lettere maiuscole e minuscole,
* ```-n``` considera numerica (invece che testuale) la chiave di ordinamento,
* ```-r``` ordina in modo decrescente,
* ```-o file``` invia l'output al file file invece che allo stdout,
* ```-t s``` usa s come separatore di campo,
* ```-k s1, s2``` usa i campi da s1 ad s2 come chiavi di ordinamento ed eventualmente i successivi (fino a fine linea) in caso di "pareggio",
* ```-s``` rende "stabile" il confronto, impedendo di ricorrere a campi ulteriori, se il confronto risulta in un pareggio sulle chiavi di ordinamento.

#### Esempi comando sort

```bash
# ordinamento delle righe del file /etc/passwd in base al terzo campo (user ID)
% sort -t: -k3 -n /etc/passwd # in questo caso basta -k3
```

### ```tr```

**Character translation**: tr è un semplice comando che permette di eseguire operazioni come la conversione di lettere minuscole in maiuscole, cancellazione della punteggiatura ecc. Siccome può prendere in input soltanto dall stdin e stampare soltanto sullo stdout, bisognerà usare pipe o delle redirezioni di input/output per farlo leggere/scrivere su file.

#### Sintassi di base

```bash
% tr str1 str2
# i caratteri in str1 vengono sostituiti con i caratteri in posizione corrispondente della stringa str2
```

#### Esempi del comando ```tr```

```bash
% tr a-z A-Z
# converte le lettere minuscole in lettere maiuscole
```

```bash
% tr -c a-zA-Z0-9 ' '
# sostituisce tutti i caratteri non alfanumerici con degli spazi, oltre a rendere tutti le lettere minuscole in lettere maiuscole (-c: complemento)
```

```bash
% tr -cs a-zA-Z0-9 ' '
# come nell'esempio precendente, ma comprime gli spazi adiacenti in un'unico spazio (opzione -s: squeeze)
```

```bash
% tr -d str
# cancella i caratteri contenuti nella stringa str
```

### Cut and paste

Il comando ```cut``` serve ad estrarre delle colonne specifiche dalle linee di testo che ricevi in input:

```bash
% cut -d: -f1 /etc/passwd # il separatore si specifica con l'opzione -d (delimiter), il campo da estrarre con l'opzione -f (field)
root
daemon
bin
```

Il comando ```paste``` combina le righe corrispondenti di due file, inserendo un delimitatore fra esse (default: ```<Tab>```):

```bash
% cd; cut -d: -f1 /etc/passwd >p1.txt; cut -d: -f6 /etc/passwd >p6.txt
% paste p1.txt p6.txt
root   /
daemon /
bin    /usr/bin
```

## Esericizi

* Scoprire quanto spazio occupa il contenuto della propria home directory. Esiste un modo per ottenere in output soltanto il numero di blocchi? (evitando di visualizzare informazioni ulteriori)?
  
  ```bash
  % du $HOME | cut -f1 | tail --lines=1
  ````

* Qual'è l'effetto del comando ```sort file >file```, dove ```file``` è il nome di un file?

  L'effetto del comando è equllo di cancellare il contenuto del file ```file```. La ridirezione dello stdout prodotto da ```sort``` sul file fornito come sorgente del testo da ordinare fa in modo che il contenuto di quest'ultimo venga cancellato ancor prima che venga letto e processato dal comando ```sort```.

* Qual'è l'effetto del comando ```tr str1 str2``` se le stringhe ```str1``` ed ```str2``` hanno lunghezze diverse.

  * Se ```str2``` è più corto di ```str1``` viene preso l'ultimo carattere di ```str2``` e viene ripetuto finché si raggiunge la lunghezza della stringa da sostituire
  * Se ```str2```è più lungo della stringa da sostituire, vengono presi solo i primi n caratteri della stringa ```str2``` (n = ```str1.length```)

* Scrivere un comando per sostituire tutti i caratteri alfanumerici nell'input con un carattere ```<Tab>```, in modo che non compaiono più ```<Tab>``` consecutivi.

  ```bash
  % tr -s 0-9A-Za-z '\t'
  ```

* Il comando ```date```fornisce data e ora su stdin. Scrivere una pipeline per estrarre soltanto i minuti.

  ```bash
  % date | cut -d\  -f4 | cut -d: -f2
  ````

* Scrivere una pipeline che permetta di scoprire se ci sono linee ripetute in un file

  ```bash
  sort file | uniq -c | sort -k1,1
  ```

* Visualizzare sullo stdout, senza ripetizioni, lo user ID di tutti gli utenti che hanno almeno un processo attivo nel sistema

  ```bash
  ps -al --no-headers | tr -s ' ' | cut -d\  -f3 | sort | uniq -d
  ```
