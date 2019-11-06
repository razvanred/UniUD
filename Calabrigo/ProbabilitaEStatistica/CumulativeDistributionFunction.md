# Funzione di ripartizione, o di distribuzione cumulativa
La funzione di distribuzione cumulativa FX(x) = F(X <= x), permette di sapere, dato un x in input data una legge di probabilità, qual'è la probabilità che succeda un evento X minore/uguale a x dato in input.\
Per esempio, nel lancio di un dado, la possibilità che il dado dia un risultato minore di 4 è uguale a 1/6 + 1/6 + 1/6 = 3/6 = 1/2.\
La f.d.c. (funzione di distribuzione cumulativa), è l'area sotto la funzione di densità da un punto -infinito ad un punto x nelle v.c. continue, e la sommatoria di tutti i valori da -infinito a x nelle v.c. discrete.

Esempio 9.1 Sia X una variabile casuale con supporto SX = [−1, 0] e funzione di densita di probabilita di forma pX (x) = cx per x ∈ SX e 0 altrove. Si
completi la definizione della funzione di densita di X, determinando il valore
della costante di normalizzazione c. Si calcoli la funzione di ripartizione di X,
esplicitandola in tutti i suoi tratti. Si ottenga il valore atteso e la varianza di
X.\

1 = integrale da -infinito a +infinito di cx = integrale da -1 a 0 di cx = c[(x^2)/2]0 - c[(x^2)/2]-1 = 0 - c/2 = -c/2 ; Quindi c = -2

pX(x) -> se x appartiene a [-1, 0], cx = -2x ; se x non appartiene a [-1, 0], cx = 0;
Abbiamo 3 casi di studio: x < -1, -1 <= x < 0, x > 0;\
* x < -1 -> integrale da -infinito a x di 0 = 0
* -1 <= x < 0 -> integrale da -1 a x di cx = c[(x^2)/2]x - c[(x^2)/2]-1 = -x^2 + 1
* x > 0 -> integrale da -infinito a -1 + integrale da -1 a 0 + integrale da 0 a x = 0 + 1 + 0 = 1

E(X) = integrale da -infinito a +infinito di x*pX(x) = integrale da -1 a 0 di x*-2x = -2[(x^3)/3]0 - -2[[(x^3)/3]]-1 = 0 - 2/3 = -2/3\
E(X^2) = integrale da -infinito a +infinito di (x^2)*pX(x) = integrale da -1 a 0 di (x^2)*-2x = -2[(x^4)/4]0 - -2[[(x^4)/4]]-1 = 0 - -2/4 = 1/2\
Var(X) = E(X^2) − (E(X))^2 = 1/2 - (-2/3)^2 = 1/2 - 4/9 = 1/18

### Modus operandi per esercizi su funzioni di ripartizione

Quindi, vediamo un procedimento da seguire nel caso ti venga chiesto di trovare la funzione di ripartizione:
* guarda bene supporto e funzione di probabilità.
* esplicita la funzione di probabilità pX(x) con i relativi casi, cioè se per esempio il supporto fosse un intervallo[a, b], dovresti guardare che probabilità potrebbe assumere x se x < a, se a <= x < b e se x > b.
* ora che hai esplicitato pX(x), puoi studiare la funzione di ripartizione PX(x), facendo l'integrale da -infinito a x, per ognuno dei casi segnati nel passo precedente, per esempio nel passo precedente dovrei fare 3 integrali diversi da -infinito ad x, dove nel primo x < 0, nel secondo .... e nel terzo ... (vedi sopra).
* Ora hai trovato FX(x), se ti vengono chiesti anche E(X) e VAR(X) trovali.