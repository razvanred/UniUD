N.B.: Consiglio di vederti gli altri file di probabilità prima di guardare gli esercizi.
# Capitolo 3
Esercizio 3.6 Da un mazzo di carte trevisane, si estrae a caso una carta. Si
calcoli la probabilità che sia o un re o una carta di denari.\
E = "la carta è un re o una carta di denari" quindi |E| = 13 ; |S| = 40\ ; esperimento = "estraggo una carta\
P(E) = |E|/|S| = 13/40\
altro metodo (usando la formula):\
P(E) = ((27 su 0) * (13 su 1)) / (40 su 1) = 13/40

Esercizio 3.7 Allo stadio di San Siro in occasione di una certa partita in tribuna nove persone su dieci sono tifosi o tifose del Milan e sempre nove persone
su dieci guadagnano piu di 50 000 Euro l’anno. Cosa si puo dedurre sulla probabilità che una persona scelta a caso tra quelle in tribuna sia un tifoso/a del
Milan che guadagna almeno 50 000 Euro l’anno?\
E = "tifoso del milan e guadagna più di 50000 euro" ; esperimento = "estrai a caso una persona" ; |S| = 10\
`P(E) = P(E1 intersecato E2) = P(E1) * P(E2) = (9/10) * (9/10) = 81/100`. Con E1 = "tifoso del milan" e E2 = "guadagna più di 50000 euro"\

Esercizio 3.8 Allo stadio di San Siro in occasione di una certa partita in tribuna due persone su dieci sono tifosi/e del Milan e una persona su dieci guadagna
meno di 50 000 Euro l’anno. Cosa si puo dedurre sulla probabilita che una persona scelta a caso tra quelle in tribuna sia o una persona tifosa del Milan oppure
guadagni meno di 50 000 Euro l’anno?\
A = "tifosi del milan", B = "guadagna meno di 50k euro"\
P(A unito B) = P(A) + P(B) - P(A intersecato B) = visto che A e B sono due eventi Indipendenti = 2/10 + 1/10 - (2/10 * 1/10) = 3/10 - 2/100 = 28/100 = 28%\

Esercizio 3.13 Si valuti in quanti modi distinti dieci persone attive in politica
possono essere scelte come ministri/e di cinque ministeri diversi.
D:10,5 = 10!/(10-5)! = 10*9*8*7*6 = 30240\
Perchè ho usato le disposizioni al posto delle combinazioni?

Esercizio 3.17 Un’urna contiene 20 palline nere e 80 palline bianche. Si estraggono in blocco 5 palline. Si calcoli la probabilità di vedere nel campione estratto 3 palline nere (e 2 bianche).\
((20 su 3) * (80 su 2)) / (100 su 5) = (1140 * 3160) / 75287520 = 0.0478 = 4.8%\

Esercizio 3.18 Si lanciano 2n monete equilibrate. Si calcoli la probabilità che
il numero delle teste sia uguale a quello delle croci.
Questo problema si traduce in: In quanti modi, lanciando 2n monete, escono fuori n teste? La soluzione è in (2n su n) modi. Poi, per ottenere la probabilità, divido (2n su n) per il numero totale di modi in cui posso lanciare 2n monete, ovvero 2^(2n). Quindi la soluzione è (2n su n) / 2^(2n). Esempio: se n = 10 (e 2n = 20), allora (20 su 10) / 2^(20) = 0.176 = 18%

# Capitolo 4
Esercizio 4.5 Da un mazzo di carte trevisane si estraggono due carte, lasciandole coperte. Si scopre la prima carta, è un asso (evento A1). Si calcoli la
probabilità che la seconda carta sia un asso. E' una probabilità condizionale?

Probabilità ipergeometriche\
Possiamo rimaneggiare il testo e farlo diventare: Qual'è la probabilità di pescare 2 assi? La probabilità è ((4 su 1) * (4 su 1)) / (40 su 2) = 0.021 = 2.1%. Questa probabilità non è una probabilità condizionale perchè scoprire la prima carta (evento A1) non ha alcun effetto sulla probabilità che anche la seconda carta sia un asso visto che l'ho gia pescata. L'evento A1, e di conseguenza l'evento A2 = "scopre la seconda carta", hanno invece effetto sulle pescate successive.


Formula probabilità composta\
Esercizio 4.8 Siano A, B, C e D eventi e A ∩ B ∩ C sia non trascurabile. Si
scriva la formula della probabilità composta per P(A ∩ B ∩ C ∩ D).\
`P(A ∩ B ∩ C ∩ D) = P(B ∩ C ∩ D) * P(A | B ∩ C ∩ D) = P(C ∩ D) * P(B | C ∩ D) * P(A | B ∩ C ∩ D) =  P(D) * P(C | D) * P(B | C ∩ D) * P(A | B ∩ C ∩ D)`.

Formula probabilità totale\
Esercizio 4.11 Si lancia un dado equilibrato. Il punteggio ottenuto dal dado
determina il numero di lanci indipendenti di una moneta equa: se il punteggio è
pari, la moneta e lanciata due volte; se e dispari, la moneta è lanciata tre volte.
Si calcoli la probabilità di ottenere due teste.

A ="esce un numero pari dal dado" (1/2), B ="esce un numero dispari dal dado" (1/2), C="escono 2 teste da 2 monete"(1/4), D="escono 2 teste su 3 monete"(1/8)\
Io so che C e D sono dipendenti rispettivamente da A e B, però sono indipendenti tra loro. `P(C intersecato D) = P(A) * P(C|A) + P(B) * (D|B) = P(A) * P(A intersecato C)/P(A) + P(B) * P(B intersecato D)/P(B) = P(A) * P(C) + P(B) * P(D) = (1/8) + (1/16) = (3/16)`

# Capitolo 5
Esercizio 5.8 (compito del 23/07/18) Un’urna contiene 20 palline nere e 80
bianche. Una seconda urna contiene 50 palline nere e 50 bianche. Una terza
urna contiene 60 palline nere e 40 bianche. Una quarta urna contiene 80 palline
nere e 20 bianche. Uno sperimentatore sceglie a caso un’urna fra le quattro con
equiprobabilita, poi estrae a caso, con reinserimento, sei palline dall’urna scelta.
Si determini la probabilita che l’urna scelta sia stata quella con 80 nere, se le
palline estratte risultano, senza tener conto dell’ordine di estrazione, cinque nere
e una bianca.\
E = "estraggo 5 palline nere e una bianca con reinserimento"
A = "estrai dalla prima urna" (1/4)\
B = "estrai dalla seconda urna" (1/4)\
C = "estrai dalla terza urna" (1/4)\
D = "estrai dalla quarta urna" (1/4)\
P(D|E) = ?\
Considero i neri come successi, e calcolo E|A, E|B, E|C, E|D:
* E|A = Bi(6, 2/10) = (6 su 5) * (2/10)^5 * (8/10)^1 = 0.001536
* E|B = Bi(6, 5/10) = (6 su 5) * (5/10)^5 * (5/10)^1 = 0.09375
* E|C = Bi(6, 6/10) = (6 su 5) * (6/10)^5 * (4/10)^1 = 0.186624
* E|D = Bi(6, 8/10) = (6 su 5) * (8/10)^5 * (2/10)^1 = 0.393216

`P(D|E) = P(E|D) * P(D) / P(E|A) * P(A) + P(E|B) * P(B) + P(E|C) * P(C + P(E|D) * P(D) = (0.393216 * (1/4)) / ((0.393216 * (1/4)) + (0.001536 * (1/4)) + (0.09375 * (1/4)) + (0.186624 * (1/4))) = 0.5824335 = 58.2%`

# Capitolo 6
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
VAR(X) = 1/k * sum(xi^2) - E(X)^2 = 1/3 * 5 - 1 = 5/3 - 3/3 = 2/3 = 0.667

# Capitolo 7
Esercizio 7.6 (compito del 18/06/18) Sia (X, Y ) una variabile casuale bivariata con componente marginale X ∼ Bi(2, 1/2) (legge binomiale con indice
n = 2 e parametro p = 1/2) e distribuzioni condizionate binomiali Y |X = x ∼
Bi(1, 1/2), per x ∈ SX . Si determinino il supporto congiunto di (X, Y ), la funzione di probabilita congiunta di (X, Y ), il supporto marginale di Y , la funzione
di probabilit`a marginale di Y . Si dica, motivando, se (X, Y ) ha componenti
indipendenti. Si calcoli infine P(X + Y = 2).

SX = {0,1,2} ; Px(0) = 1/4 ; Px(1) = 1/2 ; Px(2) = 1/4\
SY = {0,1} ; Py(0) = 1/2 ; Py(1) = 1/2\
SX,Y = {0,1,2} x {0,1} = {(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)}\
PX,Y(0,0) = PX(0) * PY(0) = 1/8
PX,Y(0,1) = PX(0) * PY(1) = 1/8
PX,Y(1,0) = PX(1) * PY(0) = 1/4
PX,Y(1,1) = PX(1) * PY(1) = 1/4
PX,Y(2,0) = PX(2) * PY(0) = 1/8
PX,Y(2,1) = PX(2) * PY(1) = 1/8

|      |   |   |    |
|:----:|:-:|:-:|:----:|
| X/Y  |Y=0|Y=1|P(X=x)|
|X=0   |1/8|1/8|2/8   |
|X=1   |1/4|1/4|2/4   |
|X=2   |1/8|1/8|2/8   |
|P(Y=y)|4/8|4/8|

PX,Y(0,0) = PX(0) * PY(0) = 2/8 * 4/8 = 1/8\
......\
PX,Y(2,0) = PX(2) * PY(0) = 4/8 * 2/8 = 1/8\
PX,Y(2,1) = PX(2) * PY(1) = 4/8 * 2/8 = 1/8\
X e Y sono indipendeti perchè PX,Y(x,y) = PX(x) * PY(y), per ogni valore di x e di y.\
P(X + Y = 2) = PX,Y(1,1) + PX,Y(2,0) = 1/4 + 1/8 = 3/8

# Capitolo 8
Esercizio 8.1 Sia X ∼ U(0, 1). Si calcolino P(X > 0), P(X < 1), P(1/4 < X <
3/4), P(X > 1/2), E(X).\
* P(X > 0)\
Se considero P(X > 0) come limitante rispetto a X strettamente maggiore di 0\
(con 0 < x < 1) integrale da x a infinito di 1/(b-a) = integrale da x a 1 di 1/(b-a) + integrale da 1 a infinito di 0 = integrale da x a 1 di [x/(b-a)]1 - [x/(b-a)]x = 1/(b-a) - x/(b-a) = (1-x)/(b-a)\
Visto che siamo nel continuo, e non nel discreto posso anche considerare P(X > 0) = P(X >= 0), quindi posso sostituire zero ad x, nella formula precedente:\
integrale da 0 a infinito di 1/(b-a) = integrale da 0 a 1 di 1/(b-a) + integrale da 1 a infinito di 0 = integrale da 0 a 1 di [x/(b-a)]1 - [x/(b-a)]0 = 1/(b-a) - 0/(b-a) = (1)/(b-a) = 1.
* P(X < 1)\
integrale da - infinito a 1 di PX(x) = integrale da - infinito a 0 di 0 + integrale da 0 a 1 di 1/(b-a) = integrale da 0 a 1 di 1/(b-a) = [x/(b-a)]1 - [x/(b-a)]0 = (1)/(b-a) = 1
* P(1/4 < X < 3/4)\
1 - (integrale da 0 a 1/4 di PX(x) + integrale da 3/4 a 1 di PX(x)) = 1 - ([x/(b-a)]1/4 - [x/(b-a)]0 + [x/(b-a)]1 - [x/(b-a)]3/4) = 1 - ((1/4 - 0) + (1/4 - 0)) = 1 - 1/2 = 1/2.
* P(X > 1/2)\
integrale da 1/2 a 1 di PX(x) + integrale da 1 a infinito di PX(x) = integrale da 1/2 a 1 di 1/(b-a) = 1/(b-a)*[x]1 - 1/(b-a)*[x]1/2 = 1 - 1/2 = 1/2.
* E(X)\
integrale tra a e b di x*PX(x) = integrale tra a e b di x/(b-a) = [x^2/2(b-a)]b - [x^2/2(b-a)]a = b^2/(2(b-a)) - a^2/(2(b-a)) = (b^2 - a^2)/(2(b-a)) = (b-a) * (b+a) / (2(b-a)) = (b+a)/2 = (a+b)/2 = 1/2.

Esercizio 8.2 La durata X di corretto funzionamento di un certo componente
ha legge esponenziale con valore atteso 6 anni. Si calcolino P(X > 6), P(X >
12), P(X > 18).\
lambda = 1/E(X) = 1/6 ; 1/6*e^-(1/6*x)\
Vediamo che, nella legge esponenziale a variabili continua, le probabilità di concentrano vicino allo zero, e mano a mano che ci allontaniamo da esso verso i positivi, le possibilità diminuiscono esponenzialmente.
* P(X > 6)\
1 - integrale da 0 a 6 di [1/6*e^-(1/6*x)] = 1 - [1/6*(e^-(x/6))/1/6]6 - [1/6*(e^-(x/6))/1/6]0 = 1 - [e^-(x/6)]6 - [e^-(x/6)]0 = 1 - 1/e - 1 = 1 - -1/e.
* P(X > 12)\
1 - integrale da 0 a 12 di [1/6*(e^-(x/6))] = 1 - [(e^-(x/6))]12 - [(e^-(x/6))]0 = 1 - 1/(e^2) - 1 = 1/(e^2).
* P(X > 18)\
1 - integrale da 0 a 18 di [1/6*(e^-(x/6))] = 1 - [(e^-(x/6))]18 - [(e^-(x/6))]0 = 1 - 1/(e^3) - 1 = 1/(e^3).


Esercizio 8.4 Sia X una v.c. continua con densita di forma pX(x) = c*e^x, se
x ∈ (−∞, 0], e zero altrove. Si determini il valore della costante c di modo
che pX(x) sia effettivamente una funzione di densita di probabilita. Si calcolino
P(0 ≤ X ≤ 2), P(−1 ≤ X ≤ 0.5) e P(X = −0.5). Infine, si ottenga il valore
atteso di X.\
1 = integrale tra -infinito e +infinito di ce^x = integrale tra -infinito e 0 di ce^x = c*[e^x]0 - c*[e^x]-infinito = c - 0 = c ; Quindi c = 1
* P(0 ≤ X ≤ 2) = integrale da 0 a 2 di e^x = 0, perchè x ∈ (−∞, 0], l'unico punto papabile sarebbe lo zero, ma nelle v.c. continue la probabilità di un punto singolo è zero.
* P(−1 ≤ X ≤ 0.5) = integrale tra -1  e 0.5 di e^x = 1 - integrale tra -1 e 0 di e^x = [e^x]0 - [e^x]-1 = e^0 - 1/e = 1 - 1/e = (e-1)/e = 63%
* P(X = −0.5) = non lo calcolo xchè nelle v.c. continue la probabilità di un punto singolo è zero.

# Capitolo 13

Esercizio 13.2 Sia X ∼ Bi(2, 0.5) Si trovi il supporto e la f.m.p. di T = 2X.\
ST = 2*2 = 4 = {0,1,2,3,4}\
fY(y) = (2nx su 2x) * (p)^2n * (1-p)^(2nx - 2x) = (ny su y) * (p)^ny * (1-p)^(ny - y)

Esercizio 13.3 Sia X ∼ Esp(1). Si ottengano supporto e f.r. di T = X2.\
ST = [0, infinito]\
g^-1(y) = y/2\
fY(y) = d/dt(FX(g^-1(y)) = fX(x) * d/dt g^-1(y) =  lambda*e^(-lambda*(x)) * \/2 = 1/2*lambda*e^(-lambda*(y))

Esercizio 13.4 Sia X ∼ Esp(λ), λ > 0. Per T =√X si ottengano ST e FT (t).\
ST = (0, infinito)\
g^-1(y) = y^2\
fT(t) = fX(g^-1(t)) * d/dt g^-1(t) = λ*e^(-λt^2) * 2t
FT(t) = 1 - P(T > t) = 1 - FT(g^-1(t)) = 1 - FT(t^2) = 1 - integrale fT(t) = 1 - 

Esercizio 13.13 (compito del 16/06/17) Una apparecchiatura ha solo due
componenti che si possono guastare. La vita operativa Xi (i = 1, 2) di ciascuna
di esse ha distribuzione esponenziale con valore atteso pari a 10 anni, indipendentemente dalla durata di corretto funzionamento dell’altra. Quando entrambe
sono guaste, l’apparecchiatura non e piu operativa. Sia T il tempo di corretto
funzionamento dell’apparecchiatura. Si esprima T come funzione di X1, X2. Si
dica qual e il supporto di T. Si ottengano poi la funzione di ripartizione e la
funzione di densita di probabilita di T, esplicitandole in tutti i loro tratti. Si
calcolino la mediana di T (e il quantile-p con p = 50/100) e P(T > 10).

Xi(i=1,2) = Esp(1/10)\
T = max(X1,X2)\
ST=[0, infinito)\
FT(t)=?, fT(t)=?\
secondo quantile=?\
P(T>10)=?

La nostra funzione di ripartizione indica la probabilità che la macchina non si guasti\
Notiamo che nel testo se tutte le resistenze sono guaste, si guasta anche la macchina. Allora per trovare la funzione di ripartizione devo fare P(X < t)
FT(t) =
* P(T <= t)
* P(X1 < t, X2 < t)
* P(X1 < t) * (X2 < t)
* (1 - e^(-lambda*x)) * (1 - e^(-lambda*x))
* (1 - e^(-x/10))^2

FT(t) =
* 0                     se x < 0
* (1 - e^(-x/10))^2     se x >= 0

fT(t) = 
* 0                                 se x < 0
* (1 - e^(-x/10))*(1/5)*e^(-x/5)   se x >= 0

secondo quantile =
* FT(t) = 0.50
* (1 - e^(-x/10))^2 = 0.50
* (1 - e^(-x/10)) = sqrt(0.50)
* e^(-x/10) = 1 - sqrt(0.50)
* -x/10 = log(1 - sqrt(0.50))
* x = -10*log(1 - sqrt(0.50))
* x = 12.2795

P(T > 10) = 
* e^(-10/10)
* e^(-1) = 0.36787


Una apparecchiatura dispone di due resistenze. La vita operativa Xi (i = 1, 2) di ciascuna di esse ha distribuzione esponenziale con valore
atteso pari a 2 anni, indipendentemente dalla durata di corretto funzionamento
dell’altra resistenza. Quando almeno una delle due resistenze e guasta, l’apparecchiatura non e piu operativa. Si supponga, per semplicita, che le resistenze
siano le uniche componenti soggette a guasto. Sia T il tempo di corretto funzionamento dell’apparecchiatura. Si esprima T come funzione di X1, X2. Si dica
qual `e il supporto di T. Si ottengano poi la funzione di ripartizione e la funzione
di densita di probabilita di T, esplicitandole in tutti i loro tratti. Si calcolino il
primo quartile di T (e il quantile-p con p = 1/4) e la probabilita condizionale
P(T > 3|T > 2).

Xi(i=1,2) ~ Esp(1/2).\
E(X) = 1/2\
T=min(X1,X2)\
St = [0, +inf)\
FT(t)=?,fT(t)=?\
primo quartile=?, P(T > 3| T > 2)=?


La nostra funzione di ripartizione indica la probabilità che la macchina non si guasti\
Notiamo che nel testo se una delle due resistenze si guasta, si guasta anche la macchina. Allora per trovare la funzione di ripartizione devo fare 1 - P(X1 > t) * P(X2 > t). Arriviamoci.\
* FT(t)
* P(T <= t)
* 1 - P(T > t)
* 1 - P(X1 > t,X2 > t)
* 1 - P(X1 > t) * P(X2 > t)
* 1 - e^(-lambda * t) * e^(-lambda * t)
* 1 - e^(-t/2) * e^(-t/2)
* 1 - e^(-t)

FT(t) = 
* 0             se x < 0
* 1 - e^(-t)    se x >= 0

fT(t) = 
* 0             se x < 0
* e^-t          se x >= 0

primo quartile
* FT(t) = 0.25
* 1 - e^(-t) = 0.25
* e^(-t) = 0.75
* -t = log(0.75)
* t = -log(0.75)
* t = 0.287682


P(T > 3 | T > 2) = 
* P(T > 3) / P(T > 2)
* e^-3 / e^-2
* 0.36787 

Una apparecchiatura dispone di tre resistenze. La vita operativa Xi (i = 1, 2, 3) di ciascuna di esse ha distribuzione uniforme continua in (0, b),
dove b > 0, con valore atteso pari a 5 anni, indipendentemente dalla durata di
corretto funzionamento delle altre resistenze. Quando tutte le tre resistenze sono guaste, l’apparecchiatura non e piu operativa. Si supponga, per semplicita,
che le resistenze siano le uniche componenti soggette a guasto. Sia T il tempo
di corretto funzionamento dell’apparecchiatura. Si esprima T come funzione
di X1, X2, X3. Si dica qual e il supporto di T. Si ottenga poi la funzione di
ripartizione di T, esplicitandola in tutti i suoi tratti. Si calcolino le probabilita
P(0 ≤ T ≤ 3) e P(T > 8).

Xi(i=1,2,3) ~ U(0, b)\
E(X) = 5\
T = max(X1,X2,X3)\
ST = (0, 10)\
FT(t)=?\
P(0<= T <= 3)=? e P(T > 8)=?

E(X) = (a+b)/2
* 5 = (0+b)/2
* b = 10

P(X1 <= t)
* integrale tra a e x di (1/(b-a))
*  x/(b-a) - a/(b-a)
*  (x-a)/(b-a)

FT(t) = P(T <= t)
* P(T <= t) = P(X1 <= t, X2 <= t, X3 <= t)
* P(X1 <= t) * (X2 <= t) * (X3 <= t)
* (t-a)^3/(b-a)^3
* (t^3)/(1000)

FT(t) = 
* 0                 se x <= 0
* (t^3)/(1000)      se 0 < x < 10
* 1                 se x >= 10

P(0 ≤ T ≤ 3) =
* P(T <= 3) - P(T <= 0)
* (3^3)/(1000) - (0^3)/(1000)
* 27/1000 - 0
* 0.027

P(T > 8) =
* 1 - (8^3)/(1000)
* 1 - (t^3)/(1000)
* 1 - 512/1000
* 0.488

# capitolo 18

Se la variabile casuale multivariata (Y1, . . . , Yn) ha componenti
indipendenti e identicamente distribuite con legge marginale normale, e in particolare Y1 ∼ N(3, 2), la variabile casuale media campionaria Y¯n =Pni=1 Yi/n ha legge normale, Y¯n ∼ N(3, 2/n). Sia n = 50. Si calcolino P(Y¯50 > 3.4) eP(Y¯50 < 2.8). Si ottenga infine il cinquantesimo percentile di Y¯50 (`e il quantilep con p = 50/100).

Y1 ~ N(3,2)\
Yn ~ N(3,2/n)\
n=50\
P(Y50 > 3.4)=?, P(Y50 < 2.8)=?\
Si ottenga infine il cinquantesimo percentile di Y¯50\
Si mostri che Yn ha legge normale

MYn(t) =
* E(e^t*Yn)
* E(e^t*(sum(Yi)/n))
* E(e^sum((t/n) * Yi))
* E(mul(e^(t/n) * Yi))
* E(mul(e^(t/n) * Y1)) // perchè Yi i.d.d
* mul(E(e^(t/n) * Y1))
* mul(MY1(t/n))
* MY1(t/n)^n
* exp(3*(t/n) + 0.5 * sqrt(2)^2 * (t/n)^2)^n
* e^((3*(t/n) + 0.5 * sqrt(2)^2 * (t/n)^2)*n)
* e^(3*t + 0.5 * sqrt(2)^2 * (t^2/n))

MYn(t) ~ N(mu, (sigma^2)/n) ~ N(3, 2/n)

P(Y50 > 3.5)= 1 - f((3.4 - 3)/1/5) = f(2) = 0.02275
P(Y50 < 2.8) = f((2.8 - 3)/1/5) = f(-1) = 1 - f(1) = 0.15866

xp = mu + sigma*zp\
xp = 3 + 1/5*z50 = 3 + 1/5 * 0 = 3