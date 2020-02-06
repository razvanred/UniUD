## Concetti utili

### metacaratteri
Metacaratteri = caratteri che la shell riconosce. Per far vedere un metacarattere come un carattere normale devo usare il backslash. Esempio: ls `file\?`\
* `*` indica una stringa di 0 o + caratteri
* `?` indica un carattere
* `[]` indica un singolo carattere tra quelli elencati tra parentesi.
Esempio: ls /dev/tty[234]. Es: ls [a-zA-Z].bak
* `{}` indica una sequenza di stringhe. Es: ls -l {file1,files}.bak
* `~user` indica la home, è come scrivere `/home/user/`
### Redirizione Input/Output
* A `>>` B e A `>` B redirezione dell'output da A a B. append e non append. Es: `echo ciao >> file`
* A `<<` B e A `<` B redirezione dell'input da B a A. Es: `wc < file`
* `|` l'output del comando precedente viene usato come input per il comando successivo.Es: `ls | more`.
* `||` esecuzione condizionale, esegue il prox comando se questo fallisce `cd dir1 || mkdir dir1`
* `&&` esecuzione condizionale, esegue il prox comando se questo ha successo `touch file.txt && echo ciao >> file.txt`
* `;` esegue un comando dopo l'altro. Es: `ls -l;cd dir;pwd`
* # History



