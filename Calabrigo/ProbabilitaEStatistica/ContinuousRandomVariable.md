# Variabile Casuale Continua univariata e bivariata
### v.c. univariate continue
Una v.c. continua si differenzia da una v.c. discreta dal fatto che la prima, prende valori infiniti compresi in un intervallo, le cui probabilità singole sono nulle, mentre la seconda prende valori finiti e con probabilità singole >= 0.\
Quella che veniva chiamata funzione di massa di probabilità per le v.c. discrete, ora si dirà funzione di densità di probabilità (f.d.p), e si indicherà nello stesso modo con PX(x).\
Leggi di densità notevoli:
* Legge uniforme continua -> `PX(x) = 1/(b-a) se x appartiene a (a,b), altrimenti PX(x) = 0`;\
Questa legge sta dicendo che la probabilità di un punto x contenuto in un intervallo [a,b], è costante per tutti i punti di quell'intervallo e uguale a 1/(b-a). Si scrive `X ~ U(a,b)`.La sua funzione di supporto è SX = [a,b].
* Legge esponenziale -> `PX(x) = λ*e^-(λ*x) se x >= 0, altrimenti PX(x) = 0`;
Le probabilità diventeranno sempre più piccole, al crescere di λ e di x. Si scrive `X ~ Esp(λ)`. La sua funzione di supporto è SX = [0,+infinito).\
P(X > x) con x > 0 = integrale definito da x a infinito di λ*e^-(λ*x) = e^-(λx), viene chiamata funzione di sopravvivenza.\
P(X <= x) con x > 0 = 1 - P(X > x), viene chiamata funzione di ripartizione.\
N.B.: Con la funzione esponenziale X ~ Esp(λ), Se cerco P(X > 3), allora P(X > 3) = λ*e^-(3λ).
### v.c. bivariate continue
Le v.c. bivariate continue lavorano su intervalli al posto che su valori discreti, quindi su spazio R^2, un intervallo possiamo immaginarcelo come un rettangolo, all'interno del quale la mia v.c. continua può assumere qualsiasi valore.\
La somma di tutte le possibilità interne al rettangolo (l'intervallo), deve essere uguale a 1, e visto che abbiamo un rettangolo con punti infiniti al suo interno, dobbiamo integrare 2 volte. Perchè 2 volte? Immagina i vertici di un rettangolo, se conosciamo 2 lati (e quindi 3 vertici) possiamo disegnarlo; quando faccio un integrale definito ho bisogno di 2 punti (integrale da A a B(....)), quindi per disegnare il rettangolo mi serviranno i lati: [A,B] e [A,C], dove A,B,C sono i vertici. Ora posso scrivere l'integrale: `(integrale da A a B(integrale da A a C(PX,Y(x,y)) dx) dy)`.

Anche alle v.c. bivariate continue posso applicare le vecchie leggi delle v.c. bivariate discrete, come le leggi marginali e le leggi condizionali, solo che al posto delle sommatorie avrò degli integrali. Posso anche calcolare la probabilità congiunta (ovvero la probabilità dell'intersezione tra X e Y).\
Esempio 8.2 Si supponga che X ∼ U(0, 1) e che per ogni x ≥ 0 si abbia Y |X = x ∼ Esp(x + 1). Il supporto congiunto di (X, Y ) è:\
SX,Y = {(x,y) appartengo a R | x appartiene a SX e y appartiene a SY|X==x} = {(x,y) appartengo a R | 0 <= x <= 1 ; y > 0}\
PX,Y(x,y) = (x+1)e^-(x+1)y se (x,y) appartiene a SX,Y; PX,Y(x,y) = 0, altrimenti.

### indipendenza di v.c. bivariate continue
I componenti di una v.c. bivariata continua sono indipendenti se PX,Y(x,y) = PX(x) * PY(y). Se è valida `(integrale da A a B(integrale da A a C(PX,Y(x,y)) dx) dy)` = `integrale da A a C (PX(x) dx) + integrale da A a B (PY(y) dy)` = `PX,Y (A ≤ X ≤ C, A ≤ Y ≤ B)`, allora X e Y sono indipendenti.