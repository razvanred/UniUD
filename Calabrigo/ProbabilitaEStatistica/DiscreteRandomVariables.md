# Cosa sono le variabili casuali?

Una variabile casuale prende lo spazio campionario e lo "digitalizza", ovvero lo trasforma da oggetti a numeri. Una volta digitalizzato lo spazio campionario, una v.c. $X$ può assumere come valore uno qualsiasi di quei numeri. Facciamo un esempio:\
Abbiamo un lancio di una moneta, lo spazio campionario è $S=\{T,C\}$; digitalizziamo $S$, e otteniamo $S'=\{0,1\}$. La mia variabile casuale $X$ potrà assumere come valore 0 o 1. Se voglio trovare la probabilità di avere una croce, scriverò: $P(X=1)=\frac 1 2$.

Essenzialmente, la differenza tra una v.c. $X$ e una variabile algebrica $x$, è che la prima può assumere solo i valori digitalizzati dello spazio campionario, mentre la seconda può assumere qualsiasi valore (nel suo dominio).

Esistono 2 tipologie di v.c.:

* variabili casuali *discrete*: possono assumere solo valori discreti, finiti.\
  Esempio: $X$ = "testa o croce"\
  Funzione di probabilità: $P(X=x_i)=P_i$\
  Funzione di ripartizione $P(X\leq x_i) = \sum\limits_{X\leq x_i} P_i$
* variabili casuali *continue*: possono assumere valori infiniti compresi tra uno o più intervalli.\
  Esempio: $X$ = "massa esatta di un qualsiasi animale di uno zoo"\
  Funzione di probabilità: $P(x_i\leq X\leq x_j)=\int_{x_i}^{x_j} f(x)\,\mathrm{d}x$\
  Funzione di ripartizione: $P(X\leq x) = \int_{-\infty}^x f(x)\,\mathrm{d}x$

## Cosa sono le funzioni di probabilità e ripartizione?

Funzione di probabilità: vedi la probabilità della variabile casuale se assume un valore specifico (nel caso delle v.c. continue, il valore specifico non esiste, e ci riferiamo ad un range di valori).

Funzione di ripartizione: vedi la probabilità che assume una variabile casuale se assume un insieme di valori, per esempio tutti i valori maggiori di un certo valore.

Cosa sono le f.m.p. (funzione di massa della probabilità) e il supporto di una v.c.?\
Una f.m.p è una funzione di probabilità che rispetta questi assiomi:

* $p_X(x)>0$
* $\sum\limits_{x\in S_X} p_X(x)=1$

Un supporto $S_X$ è lo spazio campionario digitalizzato da una v.c. $X$.

## Leggi notevoli per v.c. discrete

### Legge degenere

In una legge degenere la probabilità è concentrata in un unico punto $x_0$, e la v.c. $X$ si comporta come una costante e prende il valore di $x_0$. $P(X=x_0)=1$. Si può scrivere come $X\sim \mathcal{D}(x_0)$.

### Legge binomiale

In una legge binomiale, la probabilità di $X$ si scrive $X\sim Bi(1,\frac 1 2)$.\
Per esempio, se cerchiamo $P(X=0)$, allora $P(X=0)=\binom 1 0\cdot(\frac 1 2)^1\cdot(\frac 1 2)^0=1\cdot 0.5\cdot 1=0.5$, usando la legge binomiale: $Bi(n,p)=\binom n x\cdot p^x\cdot(1-p)^{n-x}$.

### Legge ipergeometrica

In una legge ipergeometrica la probabilità di $X$ si scrive $P(X)=IG(n,D,N)$. Dove $n$ indica quanti elementi estraggo in blocco in una volta, $D$ indica quanti elementi sono difettosi in totale, e $N$ indica quanti elementi ci sono in totale. Se cerchiamo $P(X=x)=\frac{\binom D x\cdot\binom {N-D}{n-x}}{\binom N n}.$

### Legge uniforme discreta

In una legge uniforme discreta tutte le $P(X=x)$ sono uguali a $p$. Per esempio, nel lancio di una moneta avrei $P(X=0)=\frac 1 2$ e $P(X=1)=\frac 1 2$. La legge uniforme discreta si scrive $P(X)=Ud(x_1,x_2,\ldots,x_k)$, nell'esempio del lancio della moneta scriverei $P(X)=Ud(0,1)$.

## Valore atteso e Varianza

Il valore atteso è semplicemente la media aritmetica ponderata dei valori assumibili dalla v.c:

$$
\mathrm{E}(X)=\sum\limits_{x\in S_X} x\cdot p_X(x)
$$

La varianza indica quanto lontani sono i dati dal loro centro, più grande è la varianza, maggiore è la distanza dei possibili valori della v.c. dal loro centro $\mathrm{E}(X)$.

$$
\mathrm{VAR}(X)=\sum\limits_{x\in S_X}(x-\mathrm{E}(X))^2\cdot p_X(x)\\
=\sum\limits_{x\in S_X}(x^2\cdot p_X(x))-\mathrm{E}(X)^2
$$

## V.C. bivariate

Una variabile casuale bivariata è rappresentata come $(X,Y)$, e appartiene a $\mathbb{R}^2$. Il supporto di una v.c. bivariata è $S_{x,y}$, e contiene tutte le coppie $(x,y)$ componibili. $S_x$ e $S_y$ sono detti supporti marginali di $S_{x,y}$, ovvero se io proietto tutte le ascisse dei punti $(x,y)$, ottengo  $S_x$, se proietto le ordinate ottengo $S_y$.

Immaginiamo di avere $S_{x,y}=\{(0,0),(2,0),(3,1)\}$, quindi avremo $S_x=\{0,2,3\}$, e $S_y=\{0,1\}$.

Le leggi che descrivono la probabilità di $S_x$ e $S_y$ sono dette leggi marginali. Infatti per ottenere $P(X=x)$, devo sommare tutte le probabilità dei punti $(x,y)$, che hanno $X=x$. Quindi nell'esempio di prima, per calcolare $P(Y=0)=P((0,0))+P((2,0))=\frac 1 3+\frac 1 3=\frac 2 3$

Volendo, posso scrivere le probabilità in una tabella, e la somma delle probabilità delle singole righe sono la $P(X=x)$, mentre per le colonne $P(Y=y)$.

|          |       |       |       |       |          |
| :------: | :---: | :---: | :---: | :---: | :------: |
|  $X/Y$   | $Y=0$ | $Y=1$ | $Y=2$ | $Y=3$ | $P(X=x)$ |
|  $X=0$   |  1/8  |   0   |  2/8  |   0   |   3/8    |
|  $X=1$   |   0   |   0   |  1/8  |   0   |   1/8    |
|  $X=2$   |   0   |   0   |  3/8  |   0   |   3/8    |
|  $X=3$   |   0   |   0   |   0   |  1/8  |   1/8    |
| $P(Y=y)$ |  1/8  |   0   |  6/8  |  1/8  |          |

Esempi:

* $P(X=0,Y=2)=\frac 2 8$
* $P(Y=3)=P(X=0)+P(X=1)+P(X=2)+P(X=3)=0+0+0+\frac 1 8=\frac 1 8$

## V.C. bivariate condizionali

La legge condizionale di $Y$ dato un valore osservabile di $X$ si indica con $Y\mid X=x$, dove $x\in S_X$, ha f.m.p. condizionale:

$$
p_{Y\mid X=x}(y)=P(Y=y\mid X=x)=\frac{P(X=x,Y=y)}{P(X=x)}=\frac{p_{X,Y}(x,y)}{p_X(x)}
$$

Descrive la probabilità che accada una coppia $(X,Y)=(x,y)$ fissata, diviso la probabilità che accada una qualsiasi coppia $(X,Y)$, con $x$ fissato. Nell'esempio della tabella avremmo:

$$
P(Y=2\mid X=2)=\frac{P(Y=2,X=2)}{P(X=2)}=\frac{\frac 3 8}{\frac 6 8}=\frac 3 6=\frac 1 2
$$

Questa formula è l'equivalente della probabilità condizionata: $P(A\mid B)=\frac{P(A\cap B)}{P(B)}$.

### v.c. bivariate indipendenti/dipendenti e covarianza

I componenti $X$ e $Y$ di una v.c. bivariata sono indipendenti quando la probabilità della loro intersezione è uguale al prodotto della probabilità delle componenti, per ogni $x\in X$ e $y\in Y$, scritta in formula diventa: $p_{X,Y}(x,y)=p_X(x)\cdot p_Y(y)$.

|          |       |       |          |
| :------: | :---: | :---: | :------: |
|  $X/Y$   | $Y=0$ | $Y=1$ | $P(X=x)$ |
|  $X=0$   |  1/4  |  1/4  |   1/2    |
|  $X=1$   |  1/4  |  1/4  |   1/2    |
| $P(Y=y)$ |  1/2  |  1/2  |          |

In questo esempio X e Y sono indipendenti perché $\frac 1 2\cdot\frac 1 2 = \frac 1 4$ sempre.

Si esprime la f.m.p. in forma abbreviata come applicazione che associa a $(x_i,y_j)$ il valore $p_{ij}$:

$$
S_{X,Y}\ni (x_i,y_j)\rightarrow p_{ij}=P(X=x_i,Y=y_j)\\
$$

Se $X$ e $Y$ sono dipendenti, una misura della loro distribuzione, rispetto al loro centro è la covarianza, $\mathrm{COV}(X,Y)$. La covarianza si calcola come media ponderata del prodotto di scarti delle variabili dal loro centro:

$$
\mathrm{COV}(X,Y)=\sum_i\sum_j(x_i-\mathrm{E}(X))\cdot(y_j-\mathrm{E}(Y))\cdot p_{ij})
$$
 
Visto che nella formula abbiamo due sottoformule $x_i-\mathrm{E}(X)$ e $y_j-\mathrm{E}(Y)$ che vengono moltiplicate tra loro, e possono assumere valori sia negativi che positivi, abbiamo:

* $+\cdot +=+$
* $-\cdot -=+$
* $-\cdot +=-$
* $+\cdot -=-$

Quindi, se la maggior parte degli elementi $(x,y)$ si troverà sul primo $(+\cdot +)$ o sul terzo $(-\cdot -)$ quadrante, la covarianza avrà valore positivo, altrimenti, se si troveranno sul secondo o sul quarto, la covarianza avrà valore negativo.\
Nel caso $X$ e $Y$ siano indipendenti, la covarianza avrà valore zero, $\mathrm{COV}(X,Y)=0$, perché le coppie $(x,y)$, saranno disposte tutte attorno al loro centro.

### Tipologia di esercizio da esame

Se mi viene chiesto di fare un esercizio in cui mi vengono date le leggi marginali, $X$ e $Y$, e io devo trovare $S_{X,Y}$ e $p_{X,Y}$, allora troverò prima $S_X$ e $S_Y$, applicherò le relative leggi marginali, e alla fine moltiplicherò tra loro $S_X\times S_Y$, trovando cosi $S_{X,Y}$.\
Poi calcolerò le $p_{X,Y}=p_X\cdot p_Y$, per ogni $(x,y)$ appartenente a $S_{X,Y}$ (nell'esame troverai il supporto congiunto, ovvero $(X,Y\mid X=x)$).

> Puoi considerare la formula $p_{X,Y}(x,y)=p_X(x)\cdot p_{Y\mid X}$, come $P(A\cap B)=P(B)\cdot P(A\mid B)$. Questa è la formula della probabilità congiunta (quella relativa alla tipologia di esercizio sovrastante).

Esempio:

Sia $(X,Y)$ una variabile casuale bivariata con componente marginale $X\sim Bi(1,\frac 1 2)$ (legge binomiale con indice $n=1$ e parametro $p=\frac 1 2$) e distribuzioni condizionate binomiali $Y\mid X=x\sim Bi(1,\frac 1 2)$, per $x\in S_X$. Si determinino il supporto congiunto di $(X,Y)$, la funzione di probabilità congiunta di $(X,Y)$, il supporto marginale di $Y$, la funzione di probabilità marginale di $Y$ . Si dica, motivando, se $(X,Y)$ ha componenti indipendenti. Si calcoli infine $p(X+Y=1)$.

$$
\begin{aligned}
& S_x = \{0,1\}&& p_X(0)=\frac 1 2,&& p_X(1)=\frac 1 2\\
& S_y = \{0,1\}&& p_{Y\mid X=x}(0)=\frac 1 2,&& p_{Y\mid X=x}(1)=\frac 1 2
\end{aligned}\\[.5em]
S_{X,Y}=S_x\times S_y=\{0,1\}\times\{0,1\}=\{(0,0),(0,1),(1,0),(1,1)\}
$$

$$
p_{X,Y}(0,0)=p_X(0)\cdot p_Y(0)=\frac 1 2\cdot \frac 1 2=\frac 1 4\\[.5em]
p_{X,Y}(0,1)=p_X(0)\cdot p_Y(1)=\frac 1 2\cdot \frac 1 2=\frac 1 4\\[.5em]
p_{X,Y}(1,0)=p_X(1)\cdot p_Y(0)=\frac 1 2\cdot \frac 1 2=\frac 1 4\\[.5em]
p_{X,Y}(1,1)=p_X(1)\cdot p_Y(1)=\frac 1 2\cdot \frac 1 2=\frac 1 4
$$

$X$ e $Y$ sono indipendenti perché $p_{X,Y}(x,y)=p_X(x)\cdot p_Y(y)$ per ogni valore $x$ e $y$.\
$p(X+Y=1)=p_{X,Y}(0,1)+p_{X,Y}(1,0)=\frac 1 2$
