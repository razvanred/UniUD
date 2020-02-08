## Concetti utili

### metacaratteri
Metacaratteri = caratteri che la shell riconosce. Per far vedere un metacarattere come un carattere normale devo usare il backslash. Esempio: ls `file\?`\
* `*` indica una stringa di 0 o + caratteri
* `?` indica un carattere
* `[]` indica un singolo carattere tra quelli elencati tra parentesi.
Esempio: ls /dev/tty[234]. Es: ls [a-zA-Z].bak
* `{}` indica una sequenza di stringhe. Es: ls -l {file1,files}.bak
* `~` indica la home, è come scrivere `/home/`
### Redirizione Input/Output
* A `>>` B e A `>` B redirezione dell'output da A a B. append e non append. Es: `echo ciao >> file`
* A `<<` B e A `<` B redirezione dell'input da B a A. Es: `wc < file`
* `|` l'output del comando precedente viene usato come input per il comando successivo.Es: `ls | more`.
* `||` esecuzione condizionale, esegue il prox comando se questo fallisce `cd dir1 || mkdir dir1`
* `&&` esecuzione condizionale, esegue il prox comando se questo ha successo `touch file.txt && echo ciao >> file.txt`
* `;` esegue un comando dopo l'altro. Es: `ls -l;cd dir;pwd`
### find
Il find serve a stampare a video una cartella e tutte le sue sottocartelle\
find . -name `*.c` -print\
cerca ricorsivamente a partire dalla directory corrente tutti i
file con estensione c e li stampa a video.
### grep/fgrep/egrep
grep cerca delle occorrenze all'interno di un file e le stampa a video\
fgrep rossi /etc/passwd\
fornisce in output le linee del file /etc/passwd che
contengono la stringa fissata rossi.\
grep -w print *.c\
fornisce in output le linee di tutti i file con estensione c che
contengono la parola intera print.\
 egrep ’[a-c]+z’ doc.txt\
 fornisce in output le linee del file doc.txt che contengono
una stringa che ha un prefisso di lunghezza non nulla,
costituito solo da lettere a, b, c, seguito da una z.\
### sort
serve per ordinare le linee all'interno di un file.
Esempio: file
v:b:4
e:g:98
d:h:3
sort -t: -k3,3 -n /etc/passwd , e diventa:
d:h:3
v:b:4
e:g:98
### cut e paste
cut prende una colonna di un file (le colonne vengono scelte tramite delimitatore), e la stampa a video.\
cut -d: -f1 /etc/passwd\
-d: dice che : è il delimitatore, e -f1, dice di prendere la prima colonna rispetto a quel delimitatore.\
Paste, invece, serve a combinare le colonne di 2 file, mettendole una affianco all'astra, separate da uno slash /.
### sed
Il comando sed serve a modificare i file, sostituendo le stringhe in base a certi criteri.\
Sed prevede una o più azioni. Un'azione ha questa sintassi: s/expr/str/flags dove:
* s/ indica che il carattere separatore dell'azione è /
* expr è la stringa o regex che andremo a sostituire
* str è la stringa o regex che sostituirà expr
* flag può avere vari effetti, vedi documentazione\

sed '4,5d' /etc/passwd \
stampa a video soltanto le prime 3 righe del file /etc/passwd:
d `e il comando di cancellazione che elimina dall’output tutte
le righe a partire dalla quarta ($ sta per l’ultima riga del file).\
sed 3q /etc/passwd\
stampa a video le prime tre righe
sed /sh/y/:0/_%/ /etc/passwd\
sostituisce in tutte le righe che contengono la stringa sh il
carattere : con il carattere _ ed il carattere 0 con il carattere %.\
sed s/'ciao'/'villain'/g file\
sostituisce tutte le occorrenze ciao in file con villain.\

# Script
Per avviare uno script scrivo ./nomeScript\
Per visualizzare i comandi nella shell mentre vengono eseguiti, metto come prima riga dello script set -v.
### variabili
non hanno dichiarazione di tipo, si dichiarano e assegnano nello stesso momento. x = 'cose', x = y.\
Per chiamare il valore di una variabile si usa il $: $x.\
Variabili globali: export x.\
Variabili di ambiente (sono variabili globali):
* PS1 prompt della shell
* PS2 secondo prompt della shell; utilizzato per esempio in caso di
* ridirezione dell’input dalla linea di comando
* PWD pathname assoluto della directory corrente
* UID ID dello user corrente
* PATH lista di pathname di directory in cui la shell cerca i comandi
* HOME pathanme assoluto della home directory
### Parametri e variabili di stato
$1,$2,...,$9 sono variabili associate ai primi 9 parametri che vengono passati alla shell in input.\
Esempio:\
cat > copy\
mkdir $1\
cp $2,$1\
CTRL-d\
./copy folder file\
la variabile $? ritorna il codice di errore dell'ultima operazione eseguita.\
la variabile $$ ritorna il PID della shell corrente.


