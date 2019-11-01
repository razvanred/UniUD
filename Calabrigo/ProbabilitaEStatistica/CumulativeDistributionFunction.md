# Funzione di ripartizione, o di distribuzione cumulativa
La funzione di distribuzione cumulativa FX(x) = F(X <= x), permette di sapere, dato un x in input data una legge di probabilità, qual'è la probabilità che succeda un evento X minore/uguale a x dato in input.\
Per esempio, nel lancio di un dado, la possibilità che il dado dia un risultato minore di 4 è uguale a 1/6 + 1/6 + 1/6 = 3/6 = 1/2.\
La f.d.c. (funzione di distribuzione cumulativa), è l'area sotto la funzione di densità da un punto -infinito ad un punto x nelle v.c. continue, e la sommatoria di tutti i valori da -infinito a x nelle v.c. discrete.

Esempio 9.1 Sia X una variabile casuale con supporto SX = [−1, 0] e funzione di densita di probabilita di forma pX (x) = cx per x ∈ SX e 0 altrove. Si
completi la definizione della funzione di densita di X, determinando il valore
della costante di normalizzazione c. Si calcoli la funzione di ripartizione di X,
esplicitandola in tutti i suoi tratti. Si ottenga il valore atteso e la varianza di
X.\
c = integrale da -infinito a +infinito di cx = integrale da -1 a 0 di cx = c[(x^2)/2]0 - c[(x^2)/2]-1 = 0 - c/2 = -c/2 ; Quindi c = -2
VAR(X) = E(X^2)