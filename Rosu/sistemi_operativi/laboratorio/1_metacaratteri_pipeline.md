# Metacaratteri e Redirezione dell'I/O

## Metacaratteri

Caratteri speciali riconosciuti dalla shell UNIX che possono comparire nei comandi. Quando l'utente invia un comando, la shell lo scandisce alla ricerca di eventuali metacartteri, che processa in modo speciale. Una volta processati tutti i metacaratteri, viene eseguito il comando.

Esempio:

```bash
> ls *.txt
file0.txt  file1.txt  file2.txt
```

## Abbreviazione del Pathname

I seguenti metacaratter, chiamati **wildcard**, sono usati per abbreviare il nome di un file in un pathname:

| Metacarattere | Significato                           |
|---------------|---------------------------------------|
| *             | stringa di 0 o più caratteri          |
| ?             | singolo carattere                     |
| [ ]           | singolo carattere tra quelli elencati |
| { }           | sequenza di stringhe                  |

### Esempi

```bash
> cp /JAVA/Area*.java /JAVA_backup # copia tutti i file il cui nome inizia con la stringa Area e termina con l'estensione .java nella directory JAVA_backup
```

```bash
> ls /dev/tty? # mostra i primi 10 tty
/dev/tty0  /dev/tty2  /dev/tty4  /dev/tty6  /dev/tty8
/dev/tty1  /dev/tty3  /dev/tty5  /dev/tty7  /dev/tty9
```

```bash
> ls /dev/tty[234]
/dev/tty2  /dev/tty3  /dev/tty4
```

```bash
ls /dev/tty?[234]
/dev/tty12  /dev/tty23  /dev/tty34  /dev/tty52  /dev/tty63
/dev/tty13  /dev/tty24  /dev/tty42  /dev/tty53  /dev/ttyS2
/dev/tty14  /dev/tty32  /dev/tty43  /dev/tty54  /dev/ttyS3
/dev/tty22  /dev/tty33  /dev/tty44  /dev/tty62  /dev/ttyS4
```

```bash
ls /dev/tty?[2-4]
# equivalmente al comando sopra
```

```bash
mkdir /user/studenti/rossi/{bin,doc,lib}
# crea le directory bin, doc, lib
```

## Il quoting

Il meccanismo dei **quoting** è utilizzato per inibire l'effetto dei metacaratteri. I metacaratteri a cui è applicato il quoting perdono il loro significato speciale e la shell li tratta coma caratteri ordinari.

Esistono tre meccanismi di quoting:

* **escape** ```\``` inibisce l'effetto speciale del carattere che lo segue:

```bash
> cp file file\?
> ls file*
file  'file?'
```

* tutti i metacaratteri presenti in una stringa racchiusa tra **singoli apici** non vengono processati come tali, bensì come caratteri normali:

```bash
> cat 'file?'
# mostra solo il contenuto del file file? (invece del contenuto dei file file? e file1 se presente)
```

## Redirezione dell'I/O

Di default i comandi Unix prendono l'input da **tastiera** (**standard input**) e mandano l'**output** ed eventuali **messaggi di errore** su video (**standard output, error**).

L'input/output può essere **rediretto** da/verso **file**, utilizzando opportuni metacaratteri:

| Metacarattere | Significato                                                     |
|---------------|-----------------------------------------------------------------|
| >             | redirezione dell'output                                         |
| >>            | redirezione dell'output (append)                                |
| <             | redirezione dell'input                                          |
| <<            | redirezione dell'input dalla linea di comando ("here document") |
| 2>            | redirezione dei messaggi di errore (bash Linux)                 |

Esempi:

```bash
> ls test >temp # il contenuto della cartella viene scritto sul file temp (se non esiste viene creato, se esiste viene sovrascritto)
> more temp
file1.txt
file2.txt
file3.txt
file4.txt
file5.txt
```

```bash
> echo ciao a tutti >file # redirezione dell'output
> more file
ciao a tutti
> echo ciao a tutti >>file # redirezione dell'output (modalità append)
> more file
ciao a tutti
ciao a tutti
```

Il comando ```wc``` (**word counter**) fornisce di un file numero di:

1. linee
2. parole
3. caratteri

Conta da file:

```bash
> wc <file.txt
21 42 77
> wc <file1
12 45 77
> wc <file1 <file.txt # somma tra il numero di new line, parole e caratteri dei file file1 e file.txt sommati tra loro
33 87 154
> wc <file* # identico al comando precedente se nella directory esistono solo file.txt e file1
33 87 154
```

Here document:

```bash
> wc <<delim # here document
heredoc> queste linee formano il contenuto
heredoc> del testo
heredoc> delim # comando che permette la terminazione di richiesta di input
2 7 44
```

Redirezione messaggi di errore:

```bash
> man -s2 passwd
No manual entry for passwd
See 'man 7 undocumented' for help when manual pages are not available.
> man -s2 passwd 2>temp # redirezione dei messaggi di errore
```

## Pipe

Il metacarattere **pipe** (|) serve per comporre n comandi in cascata in modo che l'output di ciascuno sia fornito in input al successivo. L'output dell'ultimo comando è l'output della pipeline.

La sequenza di comandi

```bash
> ls /usr/bin >temp
> wc -w <temp
459
```

ha lo stesso effetto della pipeline

```bash
> ls /usr/bin | wc -w
459
```

I comandi ```ls``` e ```wc``` sono eseguiti in parallelo: l'output di ```ls``` è letto da ```wc``` man mano che viene prodotto.

Per mandare in stampa la concatenazione dei file ```f1```, ```f2``` ed ```f3``` con il contenuto ordinato:

```bash
> cat f1 f2 f3 | sort >/dev/lp
```

Per visualizzare l'output di ```ls``` pagina per pagina:

```bash
> ls | more
```

## Esercizi

* Scrivere un unico comando (pipeline) per:
  * copiare il contenuto della directory ```dir1``` nella directory ```dir2```

  ```bash
  mkdir -p dir2 && cp ./dir1/* ./dir2/
  ```

  * fornire il numero di file (e directory) a cui avete accesso, contenuti ricorsivamente nella directory ```/home``` (si può usare ```ls -R```? O il comando ```find```?)

    Bisogna contare il numero di righe, non il numero di parole, perché a ciascuna riga corrisponde un file accessibile.

    ```bash
    find /home | wc -l
    ```

  * fornire la lista dei file della home directory il cui nome è una stringa di 3 caratteri seguita da una cifra

    ```bash
    ls -d /home ???[0-9] # -d visualizza solamente il contenuto della cartella attuale, non il contenuto di una cartella contenuta nella cartella attuale
    ```

* Qual'è la differenza tra i seguenti comandi?

```bash
ls # visualizza tutti i file (senza quelli nascosti che iniziano con il punto) all'interno della cartella in cui ci troviamo su stdout
ls | cat # visualizza tutto il contenuto della cartella in cascata su stdout (console), in ciascuna riga vi si trova un file
ls | more # identico a cat, ma la visualizzazione è paginata
```

* Quale effetto producono i seguenti comandi?

  * ```uniq <file``` comando che filtra le righe consecutive identiche tra di loro (stampando su stdout una sola volta)
  * ```who | wc -l``` stampa su stdout il numero di righe fornito dal comando who, ovvero il numero di utenti collegati al sistema
  * ```ps -e | wc -l``` stampa su stdout il numero di processi (più la riga di intestazione, quindi n processi + 1) che girano sulla macchina nell'instante in cui è stato eseguito il comando
