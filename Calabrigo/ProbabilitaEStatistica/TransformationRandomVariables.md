# Trasformazioni di v.c.
Innanzitutto, a cosa sono e a che cosa servono?\
Una v.c. trasformata, è una v.c. trasformata Y = g(X).\
Esempio: Se io avessi X ~ Esp(lambda), che rappresenta il tempo in attesa in minuti, e volessi trovare il tempo in attesa in secondo dovrei trovare una Y = 60*X.\
Vediamo di dare una definizione:\
Data una v.c. X ~ fX(x), una funzione g(x) e un supporto SX, chiamiamo v.c. trasformata Y, una v.c. che assume i valori Y = g(X), dove il supporto è SY, e ogni elemento del supporto di Y è derivato dalla trasfomazione di un elemento del supporto di X: SY = g(SX), e dove g(x) ha una funzione inversa g^(-1)(y) = X.\
Ora vediamo come ricavare FY(y)\
Dobbiamo distinguere 2 casi:
* se g(x) è crescente:\
FY(y) = P(Y <= y) = P(Y <= g(x)) = P(X <= g^-1(y)) = FX(g^-1(y))\
fY(y) = integrale di FX(x) = integrale di FX(g^-1(y)) = fX(x) * d/dy(g^-1(y))
* se g(x) è decrescente:\
FY(y) = P(Y <= y) = P(Y <= g(x)) = P(X > g^-1(y)) = 1 - FX(g^-1(y))\
fY(y) = integrale di 1 - FX(x) = integrale di 1 - FX(g^-1(y)) = -fX(x) * d/dy(g^-1(y))

Possiamo generalizzare la formula per trovare fY(y), sapendo che se g(x) è crescente fY(y) sarà positiva perchè d/dy(g^-1(y)) è crescente. Se fY(y) è decrescente, sarà comunque positiva perchè abbiamo un segno meno iniziale, che andrà a moltiplicare il segno meno di d/dy(g^-1(y)); xchè essendo g(x) decrescente la derivata sarà negativa. Quindi fY(y) = fX(x) * Abs(d/dy(g^-1(y)))

## METODOLOGIA:
* Per prima cosa devo identificare la/le leggi di probabilità Xi ~ fXi(x), il/i supporti SXi, e la funzione g(x). Quello che dovrò trovare, con T = g(Xi) saranno pT(t), PT(t), ST, e forse altre cose. Un altra cosa importante è verificare che g(x) sia crescente o decrescente; normalmente per verificarlo dovresti fare la derivata di fX(x) e vedere se è positiva o negativa, ma negli esercizi troverai v.c. indipendenti e con probabilità identicamente distribuita, con Max e Min. Se ho Y = Max(X1,X2), Y sarà crescente, FY(y) = P(Y <= y), xchè scelta y Max, tutte le Y che potrò scegliere saranno minori e/o uguali a y. Per il minimo vale il contrario, quindi se Y = Min(X1,X2), avrò FY(y) = 1 - P(Y > y).\
Nel caso di T ~ Esp(?), il ? lo trovo sommando i λ di A e B, quindi T ~ Esp(λ di A + λ di B).
* Ora posso calcolare la f.r. FT(t), tenendo conto che la funzione sia crescente o decrescente, e integrando di conseguenza secondo il supporto. Poi esplicita i risultati delle varie integrazioni di FT(t) con una parentesi graffa con le condizioni su t. Esempio se t > 0 -> 0, se t <= 0 -> e^-x.
* Per trovare pT(t) Posso derivare FT(t), ottenendo fT(t). Ricorda che f.r. di T è uguale alla derivata della f.r. di X, con l'osservazione che se g(x) è crescente, allora fT(t) = derivata FX(g^-1(y)), altrimenti fT(t) = derivata 1 - FX(g^-1(y)).
* Calcola le "altre cose"

Tra le "altre cose" che possono essere chieste ci sono: trova il primo quartile o una probabilità specifica, es. P(T > 3|T > 2).
# Esempio con U(a,b)
Esempio 13.9 Una apparecchiatura dispone di tre resistenze. La vita operativa Xi (i = 1, 2, 3) di ciascuna di esse ha distribuzione uniforme continua in (0, b),
dove b > 0, con valore atteso pari a 5 anni, indipendentemente dalla durata di
corretto funzionamento delle altre resistenze. Quando tutte le tre resistenze sono guaste, l’apparecchiatura non e piu operativa. Si supponga, per semplicita,
che le resistenze siano le uniche componenti soggette a guasto. Sia T il tempo di corretto funzionamento dell’apparecchiatura. Si esprima T come funzione
di X1, X2, X3. Si dica qual e il supporto di T. Si ottenga poi la funzione di
ripartizione di T, esplicitandola in tutti i suoi tratti. Si calcolino le probabilita P(0 ≤ T ≤ 3) e P(T > 8).\
T = Max(X1,X2,X3)\
X1,X2,X3 ~ U(0, b)\
E(X) = 5\
ST = (0, b]\
### Calcoliamo b usando E(X)
E(X) = 5 = integrale tra -infinito e +infinito di (x/b) = integrale tra 0 e b di (x/b) = [x^2/2b]b - [x^2/2b]0 = b^2/2b = 5 -> b^2/b = 10 -> b = 10\
ST = (0, 10]\
### Calcoliamo FT(t)
* FT(t) con t < 0 = integrale tra -infinito e t di (FX1(x) * FX2(x) * FX3(x)  (con x < 0)) = integrale ....(0) = 0\
* FT(t) (con 0 <= t < 10) = = P(T <= t) = P(X1 <= t)*P(X2 <= t)*P(X3 <= t) = P(X1 <= t)^3 = (integrale tra 0 e t di (1/b))^3 = ([x/b]t - [x/b]0)^3 = (t/10)^3\
* FT(t) (con t >= 10) = integrale tra t e infinito di (FX1(x) * FX2(x) * FX3(x)  (con t >= 10)) = integrale ....(1 * 1 * 1) = 1
### calcoliamo P(0 ≤ T ≤ 3) e P(T > 8)
P(0 ≤ T ≤ 3) = FT(3) - FT(0) = 3*3/1000 - 0 = 9/1000 = 0.027\
P(T > 8) = 1 - FT(T <= 8) = 1 -  FT(T < 0) + FT(0 <= T <= 8) = 1 - 0 - (4/5)^3 = 1 - 0 - 0.512 = 0.488

# Esempio con Esp(λ)
Esempio 13.8 Una apparecchiatura dispone di due resistenze. La vita operativa Xi (i = 1, 2) di ciascuna di esse ha distribuzione esponenziale con valore
atteso pari a 2 anni, indipendentemente dalla durata di corretto funzionamento
dell’altra resistenza. Quando almeno una delle due resistenze e guasta, l’apparecchiatura non e piu operativa. Si supponga, per semplicita, che le resistenze
siano le uniche componenti soggette a guasto. Sia T il tempo di corretto funzionamento dell’apparecchiatura. Si esprima T come funzione di X1, X2. Si dica
qual e il supporto di T. Si ottengano poi la funzione di ripartizione e la funzione
di densita di probabilita di T, esplicitandole in tutti i loro tratti. Si calcolino il
primo quartile di T (e il quantile-p con p = 1/4) e la probabilita condizionale
P(T > 3|T > 2).

E(X) = 2, quindi λ=1/2.\
Xi ~ Esp(λ). Quando una delle Xi si rompe, l'apparecchio si rompe.\
g(t) = ?\
Supporto T = ?\
FT(t) = ?, fT(t) = ?\

P(T > 3|T > 2)\
L'apparecchiatura si guasta quando una delle Xi si guasta, quindi il tempo di funzionamento dell'apparecchiatura sarà il minore tra i tempi Xi: `g(t) = min(X1,X2)`\
Il supporto di T è il minimo tra 2 Xi ~ Esp(...), quindi min([0,infinito], [0,infinito]) è  [0,infinito].\
Ora mi devo chiedere, io ho Y = g(x) = min(X1,X2). Ora Y assumerà il valore X1 ~ Esp(lambda) o X2 ~ Esp(lambda), a seconda che X1 sia minore o maggiore di X2; quindi so che Y assumerà solo uno dei due valori, e visto che X1,X2 ~ Esp(lambda), allora posso dire che sia che min(X1,X2)=X1 ~ Esp(lambda), sia che min(X1,X2)=X2 ~ Esp(lambda) -> Y ~ Esp(lambda).\
Io so che La funzione Esp è una funzione monotona decrescente, quindi:\
FT(t) = P(T <= t) = 1 - P(X1 > t, X2 > t) = 1 - P(X1 > t) * P(X2 > t) = 1 - integrale da t a infinito di (λe^(-λx)) * integrale da t a infinito di (λe^(-λx)) = 1 - (-e^(-t/2)) * (-e^(-t/2)) = 1 - e^(-t)\