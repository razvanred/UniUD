N.B.: Prima di leggere questa roba, leggi "ConcettiPreliminari.md"
# Regola di Somma (unione) e Prodotto (intersezione)
P(A unito B) = P(A) + P(B) - P(A intersecato B)\
P(A intersecato B) = P(A) + P(B) - P(A unito B)\

La probabilità di P(A intersecato B) con A e B DIPENDENTI, è P(A) * P(B)
La probabilità di P(A intersecato B) con A e B INDIPENDENTI, è P(B) * P(A dato B)
(ho generalizzato la formula dell'intersezione nel file ConcettiPreliminari.md)

# Disposizioni vs Combinazioni

La differenza tra disposizioni e combinazioni è che, mentre le disposizioni contano (x,y,z) e (x, z, y) come due elementi diversi, le combinazioni le contano come lo stesso elemento. Disposizioni e combinazioni esistono in entrambe le varianti, con o senza ripetizioni:
* Disposizioni (senza ripetizioni): n^k
* Disposizioni (con ripetizioni): n!/(n-k)!
* Combinazioni (senza ripetizioni): (n su k) (coefficiente binomiale)
* Combinazioni (con ripetizioni): (n+k-1 su k) (coefficiente binomiale)

In breve: nelle disposizioni l'ordine degli elementi non conta (a,b) diverso da (b,a), mentre nelle combinazioni l'ordine degli elementi conta (a,b) uguale a (b,a).

# Probabilità Ipergeometriche
In un lotto grande N ho D difettosi e N-D conformi. Si estraggono in blocco n pezzi, quanti difettosi posso aspettarmi?

con E = "si hanno nel campione x difettosi"
`P(E) = |E|/|S| = ((D su x) * ((N-D) su (n-x))) / (N su n)`, dove x è il numero di elementi difettosi estratti nella mia estrazione, e n-x è il numero di elementi non difettosi estratti.


# Probabilità binomiali
Dato un evento E con probabilità di successo p e (1-p) di insuccesso, una probabilità binomiale è la probabilità che E si realizzi x volte in n lanci.\
Esempio: E = "10 lanci indipendenti di m 6 monete", con p=(6/10)\
E:10,6 = (10 su 6) * (6/10)^6 * (1 - 6/10)^(10-6) = (10 su 6) * (6/10)^6 * (4/10)^(4)   //con ((10 su 6) coefficiente binomiale).\
In generale la regola è: `E:n,k = (n su k) * (p)^k * (1 - p)^(n - k)`.\
Posso vedere la formula anche come -> (numero esperimenti su numero successi) * (probabilità successo di 1 esperimento) ^ (numero successi) * (probabilità insuccesso di 1 esperimento)^(numero insuccessi).

In breve, la probabilità binomiale dice: qual'è la probabilità che un evento E, che dopo averlo eseguito n volte dia k successi?

# Probabilità condizionale
La probabilità condizionale, dati 2 eventi A e B in relazione A|B (si legge A dato B), va a vedere cos'è A, dando per scontato che B è avvenuto. la probabilità P(A|B), è la probabilità che avvenga A, dando per scontato che B sia gia avvenuto.\
`P(E|A) = P(E intersecato A)/P(A)`\
Esempio: Io ho 2 eventi A = "sono donne" (1/3), B = "alcolizzate"(1/10). Voglio trovare sia la probabilità che prendendo a caso una persona, essa sia una donna alcolizzata, che la probabilità che prendendo a caso una donna, essa sia alcolizzata:.\
P(B intersecato A) = P(A) * P(B dato A) = (visto che A e B sono eventi indipendenti) = P(A) * P(B) = 1/30 -> prendendo a caso una persona, qual'è la probabilità che sia alcolizzata?\
B dato A = P(B intersecato A) / P(A) = (1/30) / (1/3) =  1/10 -> prendendo a caso una donna, qual'è la probabilità che sia alcolizzata?\
Se volessi trovare gli uomini alcolizzati farei: C = "sono uomini" (2/3). P(B | C) = P(B intersecato C) / P(C) = (visto che B e C sono eventi indipendenti) = (P(B) * P(C)) / P(C) = P(B) = 1/10

# Formula della probabilità totale
La formula della probabilità totale dice che P(A U B) = P(A) + P(B) - P(A intersecato B).



# Formula di bayes
La formula di bayes serve per calcolare B|A avendo A|B:\
P(B|A) = P(A|B) * P(B) / sum(P(A|Ai) * P(Ai)), dove Ai sono tutti gli eventi dipendenti da A.