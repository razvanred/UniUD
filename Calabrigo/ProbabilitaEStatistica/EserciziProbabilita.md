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
