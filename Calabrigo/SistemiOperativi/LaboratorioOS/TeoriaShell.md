# Teoria Shell
## metacaratteri
Metacaratteri = caratteri che la shell riconosce. Per far vedere un metacarattere come un carattere normale devo usare il backslash. Esempio: ls `file\?`\
* `*` indica una stringa di 0 o + caratteri
* `?` indica un carattere
* `[]` indica un singolo carattere tra quelli elencati tra parentesi.
Esempio: ls /dev/tty[234]. Es: ls [a-zA-Z].bak
* `{}` indica una sequenza di stringhe. Es: ls -l {file1,files}.bak
* `~` indica la home, è come scrivere `/home/`
## Redirizione Input/Output
* A `>>` B e A `>` B redirezione dell'output da A a B. append e non append. Es: `echo ciao >> file`
* A `<<` B e A `<` B redirezione dell'input da B a A. Es: `wc < file`
* `|` l'output del comando precedente viene usato come input per il comando successivo.Es: `ls | more`.
* `||` esecuzione condizionale, esegue il prox comando se questo fallisce `cd dir1 || mkdir dir1`
* `&&` esecuzione condizionale, esegue il prox comando se questo ha successo `touch file.txt && echo ciao >> file.txt`
* `;` esegue un comando dopo l'altro. Es: `ls -l;cd dir;pwd`
## find
Il find serve a stampare a video una cartella e tutte le sue sottocartelle\
find . -name `*.c` -print\
cerca ricorsivamente a partire dalla directory corrente tutti i
file con estensione c e li stampa a video.
find /etc -type d -ls
cerca ricorsivamente a partire dalla directory /etc tutte e solo
le sottodirectory, applicando il comando ls ad ognuna.
## grep/fgrep/egrep
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
## sort
serve per ordinare le linee all'interno di un file.
Esempio: file\
v : b:4\
e:g:98\
d:h:3\
sort -t: -k3,3 -n /etc/passwd , e diventa:\
d:h:3\
v : b:4\
e:g:98
## cut e paste
cut prende una colonna di un file (le colonne vengono scelte tramite delimitatore), e la stampa a video.\
cut -d: -f1 /etc/passwd\
-d: dice che : è il delimitatore, e -f1, dice di prendere la prima colonna rispetto a quel delimitatore.\
Paste, invece, serve a combinare le colonne di 2 file, mettendole una affianco all'astra, separate da uno slash /.
## sed
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
sostituisce tutte le occorrenze ciao in file con villain.

Esempio:\
cat /etc/passwd | sed ’s?/bin/.*sh$ ?/usr/local&?’\
cerca tutte le righe in input in cui compare la stringa corrispondente
all’espressione regolare /bin/.*sh$ (ad esempio /bin/bash) e
sostituisce quest’ultima con la stringa corrispondente a
/usr/local/bin/.*sh$ (ad esempio /usr/local/bin/bash). Si
noti che, siccome il carattere separatore di sed compare nella
stringa da cercare, si `e usato il carattere ? come separatore. Inoltre
il carattere & viene rimpiazzato automaticamente da sed con la
stringa cercata (corrispondente a /bin/.*sh$).

Esempio:\
sed ’/^root/,/^bin/s/: x :/::/w disabled.txt’ /etc/passwd\
sostituisce la password criptata (rappresentata dalla x) con la
stringa vuota nelle righe in input comprese fra quella che inizia con
root e quella che inizia con bin; tali righe sono poi accodate nel
file disabled.txt.

Esempio:\
sed "s?//?linea di commento del file $1:?w $2" -n $1\
Scrivere uno script che estragga soltanto i commenti dal file con estensione
java fornito come primo argomento, sostituendo // con la stringa linea
di commento del file <nome del file>:. Inoltre i commenti estratti
devono essere salvati nel file fornito come secondo argomento.

# Script
Per avviare uno script scrivo ./nomeScript\
Per visualizzare i comandi nella shell mentre vengono eseguiti, metto come prima riga dello script set -v.
## variabili
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
## Parametri e variabili di stato
$1,$2,...,$9 sono variabili associate ai primi 9 parametri che vengono passati alla shell in input.\
Esempio:\
cat > copy\
mkdir $1\
cp $2,$1\
CTRL-d\
./copy folder file\
la variabile $? ritorna il codice di errore dell'ultima operazione eseguita.\
la variabile $$ ritorna il PID della shell corrente.
## read
La variabile restofline è un variabile di default, ed è un vettore di stringhe.\
Il comando read legge una linea dello stdin e prende come parametri n variabili e restofline. Si comporta come uno scanf che legge una riga e la mette in un vettore di stringhe.\
Esempio:

shell:\
./esempio.sh\
ciao sono tizio

esempio .sh:\
read var1 restofline\
echo $var1 #ciao\
echo $restofline #sono tizio
## if/else e test
if condizione
then\
    successCond\
else\
    failureCond\
fi

La condizione nell'if è un qualsiasi comando che poi darà un codice di errore, se il codice di errore è zero allora eseguo la successCond, altrimenti eseguo la failureCond.\
In alternativa posso usare il comando test [expression].\
espressioni che controllano se un file possiede certi attributi:\
* -e f restituisce vero se f esiste;
* -f f restituisce vero se f esiste ed è un file ordinario;
* -d f restituisce vero se f esiste ed è una directory;
* -r f restituisce vero se f esiste ed è leggibile dall’utente;
* -w f restituisce vero se f esiste ed è scrivibile dall’utente;
* -x f restituisce vero se f esiste ed è eseguibile dall’utente;

espressioni su stringhe:
* -z str restituisce vero se str `e di lunghezza zero;
* -n str restituisce vero se str non `e di lunghezza zero;
* str1 = str2 restituisce vero se str1 `e uguale a str2;
* str1 != str2 restituisce vero se str1 `e diversa da str2;

I espressioni su valori numerici:
* num1 -eq num2 restituisce vero se num1 `e uguale a num2;
* num1 -ne num2 restituisce vero se num1 non `e uguale a num2;
* num1 -lt num2 restituisce vero se num1 `e minore di num2;
* num1 -gt num2 restituisce vero se num1 `e maggiore di num2;
* num1 -le num2 restituisce vero se num1 `e minore o uguale a num2;
* num1 -ge num2 restituisce vero se num1 `e maggiore o uguale a num2

Esempio:\
x=2\
if test $x -eq 2\
then\
echo uguale\
else\
echo diverso\
fi\
exit 0\
Ctrl-d
## cicli
Ciclo tradizionale, commands vengono eseguiti finchè la condition_command è vera.\
while condition_command\
do\
    commands\
done\
Esempio ciclo while con contatore k fino a 10:\
k=0\
while test k -lt 10\
do\
echo $k\
k = $((k+1))\
done\

Ciclo until, continua ad eseguire commands finchè condition_command è falsa.\
until condition_command\
do\
commands\
done

Ciclo for, continua ad eseguire il ciclo per tutti gli elementi in wordlist (è tipo un foreach)\
for var in wordlist\
do\
commands\
done\
Esempio:\
for i in 1 2 3 4 5\
do\
echo the value of i is $i\
done

Case selection, è come lo switch case\
case $# in\
1)\ #case 1
cat >>$1\
;;\
2)\ #case 2
cat >>$1 <$2\
;;\
*)\ #default
echo "usage: append out_file [in_file]"\
;;\
esac\
exit 0
## Command Sobstitution
Il meccanismo di command substitution permette di sostituire ad un comando o pipeline quanto stampato sullo standard output da quest’ultimo.
Esempi:\
date\
Tue Nov 19 17:50:10 2002\
vardata=‘date‘\
echo $vardata\
Tue Nov 19 17:51:28 2002

# Syntax
Per applicare un comando ad ogni file di una directory, uso il simbolo /*.\
Per esempio: wc -c directory/*\
Se devo fare un test per più cose, per esempio vedere se un file è ordinario (-f) e leggibile (-r) devo usare -a per verificare entrambe. Esempio:\
if test -r $i -a -f $i # sarebbe come scrivere test -e -f $i, ma per la sintassi cosi non funziona.

N.B.: Quando si fanno le operazioni tra interi, bisogna metterele sotto doppia parentesi tonda con il dollaro. Esempio:\
k=0\
k=$((k+5)) #k=5\
k+=5 #k=05

N.B.: quando faccio un for in una directory, devo scrivere vettore/*\
Esempio:\
for i in /home/*\
do\
...\
done

N.B.: quando scrivo un comando c='cat file | cut -d: -f2', che ritorna un vettore di stringhe, posso iterarlo facendo:\
for i in c*\
do\
...\
done

N.B: tutti i comandi si trovano nella cartella /bin/

N.B: se in uno SCRIPT ricevi degli input ricorda di controllare sia il numero di parametri, che il contenuto dei singolo parametri:\
if test $# -ne 1\
then\
echo ...\
exit 1\
fi\
if ! test -d $1 #se $1 non è una cartella\
then\
echo ...\
exit 1\
fi