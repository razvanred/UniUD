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

Cosa sono le f.m.p. (funzione di massa della probabilità) e il supporto di una v.c.?\
Una f.m.p è una funzione di probabilità che rispetta questi assiomi:
* p(x) > 0
* sum(p(x)) = 1

Un supporto Sx è lo spazio campionario digitalizzato da una v.c. X.

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

# V.c. bivariate
Una variabile casuale bivariata è rappresentata come (X,Y), e appartiene a R^2. Il supporto di una v.c. bivariata è Sx,y, e contiene tutte le coppie (x,y) componibili. Sx e Sy sono detti supporti marginali di Sx,y, ovvero se io proietto tutte le ascisse dei punti (x,y), ottengo  Sx, se proietto le ordinate ottengo Sy.\
Immaginiamo di avere Sx,y={(0,0),(2,0),(3,1)}, quindi avremo Sx = {0,2,3}, e Sy = {0,1}.\
Le leggi che descrivono la probabilità di Sx e Sy sono dette leggi marginali. Infatti per ottenere la P(X=x), devo sommare tutte le probabilità dei punti (x,y), che hanno X=x. Quindi nell'esempio di prima, per calcolare Py(Y = 0) = P((0,0)) + P((2,0)) = 1/3 + 1/3 = 2/3\
Volendo posso scrivere le probabilità in una tabella, e la somma delle probabilità delle singole righe sono la P(X = x), mentre per le colonne P(Y = y).
|      |   |   |   |   |      |
|:----:|:-:|:-:|:-:|:-:|:----:|
| X/Y  |Y=0|Y=1|Y=2|Y=3|P(X=x)|
|X=0   |1/8| 0 |2/8| 0 |3/8   |
|X=1   | 0 | 0 |1/8| 0 |1/8   |
|X=2   | 0 | 0 |3/8| 0 |3/8   |
|X=3   | 0 | 0 | 0 |1/8|1/8   |
|P(Y=y)|1/8| 0 |6/8|1/8|
Esempi:
* `P(X = 0, Y = 2) = 2/8`.
* `P(Y = 3) = P(X=0)+P(X=1)+P(X=2)+P(X=3) = 0+0+0+1/8 = 1/8`.\

Le probabilità condizionate con le v.c. bivariate P(X = x | Y = y) = P(X=x,Y=y) / P(X=x), ovvero la probabilità che accada una coppia (X,Y) = (x,y) fissata, diviso la probabilità che accada una qualsiasi coppia (X,Y), con x fissato, ma per qualunque y appartenente a Sy. Nell'esempio della tabella avremmo:\
`P(Y=2|X=2) = P(Y=2,X=2)/P(Y=2) = 3/8 / 6/8 = 3/6 = 1/2`.\
Questa formula, `P(X = x | Y = y) = P(X=x,Y=y) / P(X=x)`, è la stessa della probabilità condizionata: `P(A|B) = P(A intersecato B) * P(A)`

### v.c. bivariate indipendenti/dipendenti e covarianza
I componenti X e Y di una v.c. bivariata sono indipendenti quando la probabilità della loro intersezione è uguale al prodotto della probabilità delle componenti, per ogni x che appartiene ad X e y che appartiene ad Y, scritta in formula diventa: `fx,y(x,y) = fx(x) * fy(y)`.
|    |    |    |    |
|:--:|:--:|:--:|:--:|
| X/Y  |Y=0|Y=1|P(X=x)|
|X=0   | 1/4|1/4 |1/2 |
|X=0   |1/4 |1/4 |1/2 |
|P(Y=y)  |1/2 |1/2 |
In questo esempio X e Y sono indipendenti perchè 1/2 * 1/2 = 1/4 sempre.

Se X e Y sono dipendenti, una misura della loro distribuzione, rispetto al loro centro è la covarianza, COV(X,Y). La Covarianza si calcola come media ponderata del prodotto di scarti delle variabili dal loro centro `COV(X,Y) = sumi(sumj(xi - E(X))* (yj - E(Y)) * pi,j))`. Visto che nella formula abbiamo 2 sottoformule (xi - E(X)) e (yj - E(Y)), che vengono moltiplicate tra loro e possono assumere valori sia negativi che positivi, abbiamo:
* +*+ = +
* -*- = +
* -*+ = -
* +*- = -

Quindi, se la maggior parte degli elementi (x,y) si troverà sul primo (+*+) o sul terzo(-*-) quadrante, la covarianza avrà valore positivo, altrimenti, se si troveranno sul secondo o sul quarto, la covarianza avrà valore negativo.\
Nel caso X e Y siano indipendenti, la covarianza avrà valore zero, COV(X,Y) = 0, xchè le coppie (x,y), saranno disposte tutte attorno al loro centro.

N.B.: (TIPOLOGIA DI ESERCIZIO DA ESAME): Se mi viene chiesto di fare un esercizio in cui mi vengono date le leggi marginali, X e Y, e io devo trovare Sx,y e px,y, allora troverò prima Sx e Sy, applicherò le relative leggi marginali, e alla fine moltiplicherò tra loro Sx * Sy, trovando cosi Sx,y. Poi calcolerò le px,y = px * py, per ogni (x,y) appartenente a Sx,y. (nell'esame troverai il supporto congiunto, ovvero (X,Y|X=x)).\
N.B: Puoi considerare la formula PX,Y(x,y) = PX(x) * PY|X=x(y), come P(A intersecato B) = P(A|B) * P(A). Questa è la formula della probabilità congiunta (quella relativa alla tipologia di esercizio sovrastante).

Esempio:
 Sia (X, Y ) una variabile casuale bivariata con componente marginale X ∼ Bi(1, 1/2) (legge binomiale con indice n = 1 e parametro p = 1/2) e
distribuzioni condizionate binomiali Y |X = x ∼ Bi(1, 1/2), per x ∈ SX . Si determinino il supporto congiunto di (X, Y ), la funzione di probabilit`a congiunta
di (X, Y ), il supporto marginale di Y , la funzione di probabilit`a marginale di
Y . Si dica, motivando, se (X, Y ) ha componenti indipendenti. Si calcoli infine P(X + Y = 1).
Sx = {0,1} Px(0) = 1/2, Px(1) = 1/2\
Sy = {0,1} PyconX=x(0) = 1/2, PyconX=x(1) = 1/2\
Sx,y = Sx x Sy = {0,1} x {0,1} = {(0,0),(0,1),(1,0),(1,1)}\
Px,y(0,0) = Px(0) * Py(0) = 1/2 * 1/2 = 1/4\
Px,y(0,1) = Px(0) * Py(1) = 1/2 * 1/2 = 1/4\
Px,y(1,0) = Px(1) * Py(0) = 1/2 * 1/2 = 1/4\
Px,y(1,1) = Px(1) * Py(1) = 1/2 * 1/2 = 1/4\
X e Y sono indipendenti perchè Px,y(x,y) = Px(x) * Py(y) per ogni valore x e y.\
P(X + Y = 1) = Px,y(0,1) + Px,y(1,0) = 1/2