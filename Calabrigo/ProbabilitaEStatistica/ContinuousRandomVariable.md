# Variabile Casuale Continua univariata e bivariata

## v.c. univariate continue

Una v.c. continua si differenzia da una v.c. discreta dal fatto che la prima prende valori infiniti compresi in un intervallo, le cui probabilità singole sono nulle, mentre la seconda prende valori finiti e con probabilità singole $\geq 0$.

Quella che veniva chiamata funzione di massa di probabilità per le v.c. discrete, ora si dirà funzione di densità di probabilità (f.d.p), e si indicherà nello stesso modo con $P_X(x)$.

Leggi di densità notevoli:

### Legge uniforme continua
  
$P_X(x)=\frac{1}{b-a}$ se $x$ appartiene a $(a,b)$, altrimenti $P_X(x)=0$
Questa legge sta dicendo che la probabilità di un punto x contenuto in un intervallo $[a,b]$, è costante per tutti i punti di quell'intervallo e uguale a $\frac {1}{b-a}$. Si scrive $X\sim U(a,b)$. La sua funzione di supporto è $S_X=[a,b]$.

### Legge esponenziale
  
$P_X(x)=\lambda\cdot e^{-(λx)}$ se $x\geq 0$, altrimenti $P_X(0)=0$

Le probabilità diventeranno sempre più piccole, al crescere di $\lambda$ e di $x$. Si scrive $X\sim\mathrm{Esp}(\lambda)$. La sua funzione di supporto è $S_X=[0,+\infty[$.

* $P(X>x)$ con $x>0=\int_x^{\inf} λe^{-(λx)}\,\mathrm{d}x=e^{-(λx)}$, viene chiamata funzione di sopravvivenza.
* $P(X\leq x)$ con $x>0=1-P(X>x)$, viene chiamata funzione di ripartizione.

Con la funzione esponenziale $X\sim\mathrm{Esp}(λ)$, Se cerco $P(X>3)$, allora $P(X>3)=λ*e^{-(3λ)}$.

## v.c. bivariate continue

Le v.c. bivariate continue lavorano su intervalli al posto che su valori discreti, quindi su spazio $\mathbb{R}^2$, un intervallo possiamo immaginarcelo come un rettangolo, all'interno del quale la mia v.c. continua può assumere qualsiasi valore.\
La somma di tutte le possibilità interne al rettangolo (l'intervallo), deve essere uguale a 1, e visto che abbiamo un rettangolo con punti infiniti al suo interno, dobbiamo integrare 2 volte. Perché 2 volte? Immagina i vertici di un rettangolo, se conosciamo 2 lati (e quindi 3 vertici) possiamo disegnarlo; quando faccio un integrale definito ho bisogno di 2 punti (integrale da A a B(....)), quindi per disegnare il rettangolo mi serviranno i lati: [A,B] e [A,C], dove A,B,C sono i vertici. Ora posso scrivere l'integrale:

$$
\int_A^B\left(\int_A^CP_{X,Y}(x,y)\,\mathrm{d}x\right)\,\mathrm{d}y
$$

Anche alle v.c. bivariate continue posso applicare le vecchie leggi delle v.c. bivariate discrete, come le leggi marginali e le leggi condizionali, solo che al posto delle sommatorie avrò degli integrali. Posso anche calcolare la probabilità congiunta (ovvero la probabilità dell'intersezione tra X e Y).

Esempio 8.2

Si supponga che $X\sim U(0,1)$ e che per ogni $x\geq0$ si abbia $Y |X=x\sim\mathrm{Esp}(x+1)$. Il supporto congiunto di $(X,Y)$ è:

$S_{X,Y}=\{(x,y)\in\mathbb{R}\mid x\in S_X,\,y\in S_{Y|X=x}\}=\{(x,y)\in\mathbb{R}\mid 0\leq x\leq 1,\,y>0\}$

$P_{X,Y}(x,y)=(x+1)e^{-(x+1)y}$ se $(x,y)\in S_{X,Y}$,\
$P_{X,Y}(x,y)=0$ altrimenti

## Indipendenza di v.c. bivariate continue

I componenti di una v.c. bivariata continua sono indipendenti se $P_{X,Y}(x,y)=P_X(x)\cdot P_Y(y)$. Se è valida $\int_A^B(\int_A^C(P_{X,Y}(x,y)) dx) dy$=$\int_A^C(P_X(x) dx)+\int_A^B(P_Y(y) dy)$ = $P_{X,Y}(A\leq X\leq C, A\leq Y\leq B)$, allora $X$ e $Y$ sono indipendenti.
