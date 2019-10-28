# Cosa sono le variabili casuali?
Una variabile casuale prende lo spazio campionario e lo "digitalizza", ovvero lo trasforma da oggetti a numeri. Una volta digitalizzato lo spazio campionario una v.c. X piò assumere come valore uno qualsiasi di quei numeri. Facciamo un esempio:\
Abbiamo un lancio di una moneta, lo spazio campionario è S={T, C}; digitalizziamo S, e otteniamo S'={0,1}. La mia variabile casuale X potrà assumere come valore 0 o 1. Se voglio trovare la probabilità di avere una croce, scriverò: `P(X==1) = 1/2`.\

Essenzialmente, la differenza tra una v.c. X e una variabile algebrica x, è che la prima può assumere solo i valori digitalizzati dello spazio campionario, mentre la seconda può assumere qualsiasi valore (nel suo dominio).

Esistono 2 tipologie di v.c.:
* variabili casuali discrete: possono assumere solo valori discreti, finiti.\
Esempio: X = "testa o croce"\
Funzione di probabilità: P(X = xi) = pi\
Funzione di ripartizione P(X <= xi) = sum per tutte le X <= xi di pi
* variabili casuali continue: possono assumere valori infiniti compresi tra uno, o più intervalli.\
Esempio: X = "massa esatta di un qualsiasi animale di uno zoo"\
Funzione di probabilità: P(xi <= X <= xj) = integrale da xi a xj di f(x) dx\
Funzione di ripartizione: P(X <= x) = integrale da - infinito a x di f(x) dx\

Cosa sono le funzioni di probabilità e ripartizione?\
Funzione di probabilità posso vederla come: vedi la probabilità della variabile casuale se assume un valore specifico (nel caso delle v.c. continue, il valore specifico non esiste, e ci riferiamo ad un range di valori).\
Funzione di ripartizione: vedi la probabilità che assume una variabile casuale se assume un insieme di valori, per esempio tutti i valori maggiori di un certo valore.

# Leggi notevoli per v.c. discrete
* Legge degenere\
In una legge degenere la probabilità è concentrata in un unico punto x0, e la v.c. X si comporta come una costante e prende il valore di x0. P(X=x0) = 1. Si può scrivere come X ∼ D(x0).
* Legge binomiale\
In una legge binomiale, la probabilità di X si scrive P(X) ∼ Bi(1, 0.5). Per esempio, se cerchiamo P(X = 0), allora `P(X = 0) = (1 su 0) * (0.5)^1 * (0.5)^0 = 1 * 0.5 * 1 = 0.5`, usando la legge binomiale: Bi(n, p) = (n su x) * p^x * (1-p)^(n-x), con x = 0
* Legge ipergeometrica\
In una legge ipergeometrica la probabilità di X si scrive P(X) = IG(n, D, N). Dove n indica quanti elementi estraggo in blocco in una volta, D indica quanti elementi sono difettosi in totale, e N indica quanti elementi ci sono in totale. Se cerchiamo P(X = x) = ((D su x) * (N - D su n - x)) / (N su n).\
* Legge uniforme discreta\
In una legge uniforme discreta tutte le P(X = x) sono uguali a p. per esempio nel lancio di una moneta avrei P(X = 0) = 1/2 e P(X = 1) = 1/2. La legge uniforme discreta si scrive P(X) = Ud(x1,x2,...,xk), nell'esempio del lancio della moneta scriverei P(X) = Ud(0,1).s

# Valore atteso e Varianza
Il valore atteso e semplicemente la media aritmetica ponderata dei valori assumibili dalla v.c -> `E(X) = sum per tutte le i(xi * pi)`\
La varianza indica quanto lontani sono i dati dal loro centro, più grande è la varianza, maggiore è la distanza dei possibili valori della v.c. dal loro centro E(X). Ecco la formula. -> `VAR(X) = sum per tutte le i((xi - E(X))^2 * pi)`, un'altra formula, ridotta, per la varianza è: `VAR(X) = sum per tutte le i((xi^2 * pi) - E(X)^2)`



# Esercizi
Esercizio 6.4 Si indaghi se le v.c. univariate X ∼ Bi(2, 0.5) e Y ∼ U d(0, 1, 2)
sono identicamente distribuite.

X ∼ Bi(2, 0.5)\
P(X = 0) = (2 su 0) * (0.5)^0 * (0.5)^2 = 1/4\
P(X = 0) = (2 su 1) * (0.5)^1 * (0.5)^1 = 1/2\
P(X = 0) = (2 su 2) * (0.5)^2 * (0.5)^0 = 1/4\

Y ∼ U d(0, 1, 2)\
P(Y = 0) = 1/3\
P(Y = 1) = 1/3\
P(Y = 2) = 1/3\

X e Y non sono identicamente distribuite


Esercizio 6.5 Sia X ∼ Ud(x1, . . . , xk). Si calcolino E(X) e Var(X) (prova con k = 3 Ud(0,1,2))\
E(X) = sum(xi * pi) = 1/k * sum(xi) = 1/3 * 3 = 1
VAR(X) = 1/k * sum(xi^2) - E(X)^2 = 1/3 * 5 - 1 = 5/3 - 3/3 = 2/3 = 0.667\