# Probabilità Classica

## Regola di Somma (unione) e Prodotto (intersezione)

$$
P(A\cup B) = P(A) + P(B) - P(A\cap B)\\
P(A\cap B) = P(A) + P(B) - P(A\cup B)
$$

La probabilità di $P(A\cap B)$ con $A$ e $B$ *dipendenti*, è $P(A)\cdot P(B)$.\
La probabilità di $P(A\cap B)$ con $A$ e $B$ *indipendenti*, è $P(B)\cdot P(A\mid B)$ (ho generalizzato la formula dell'intersezione in *Concetti Preliminari*)

## Disposizioni vs Combinazioni

La differenza tra disposizioni e combinazioni è che, mentre le disposizioni contano $(x,y,z)$ e $(x,z,y)$ come due elementi diversi, le combinazioni le contano come lo stesso elemento. Disposizioni e combinazioni esistono in entrambe le varianti, con o senza ripetizioni:

* Disposizioni con ripetizioni: $n^k$
* Disposizioni senza ripetizioni (semplici): $\frac{n!}{(n-k)!}$
* Combinazioni con ripetizioni: $\binom{n+k-1}k$
* Combinazioni senza ripetizioni (semplici): $\binom{n}k$

In breve: nelle disposizioni l'ordine degli elementi non conta $(a,b)$ diverso da $(b,a)$, mentre nelle combinazioni l'ordine degli elementi conta $(a,b)$ uguale a $(b,a)$.

## Probabilità Ipergeometriche

In un lotto grande $N$ ho $D$ difettosi e $N-D$ conformi. Si estraggono in blocco $n$ pezzi, qual'è la probabilità di averne $x$ difettosi?

Con $E$ = "si hanno nel campione $x$ difettosi"
$P(E)=\frac{|E|}{|S|}=\frac{\binom D x\cdot\binom{N-D}{n-x}}{\binom N n}$, dove $x$ è il numero di elementi difettosi estratti nella mia estrazione, e $n-x$ è il numero di elementi non difettosi estratti.

## Probabilità binomiali

Dato un evento $E$ con probabilità di successo $p$ e $1-p$ di insuccesso, una probabilità binomiale è la probabilità che $E$ si realizzi $x$ volte in $n$ lanci. In generale la regola è:

$$
E:n,k = \binom n k\cdot p^k\cdot(1-p)^{n-k}
$$

Posso vedere la formula anche come:

$$
\binom{\text{numero esperimenti}}{\text{numero successi}}\cdot(p\text{ successo di 1 esperimento})^{(\text{numero successi})}\cdot(p\text{ insuccesso di 1 esperimento})^{(\text{numero insuccessi})}.
$$

In breve, la probabilità binomiale dice: qual'è la probabilità che un evento $E$, dopo averlo eseguito $n$ volte, dia $k$ successi?

### Esempio

$E$ = "10 lanci indipendenti di 6 monete", con $p$=$\frac{6}{10}$.\
$E:10,6=\binom{10}{6}\cdot(\frac{6}{10})^6\cdot(1 - \frac{6}{10})^{10-6}=\binom{10}{6}\cdot(\frac{6}{10})^6\cdot(\frac{4}{10})^4$.

## Probabilità condizionale

La probabilità condizionale, dati 2 eventi $A$ e $B$ in relazione $A\mid B$ (si legge $A$ dato $B$), va a vedere cos'è $A$, dando per scontato che $B$ è avvenuto. La probabilità $P(A\mid B)$, è la probabilità che avvenga $A$, dando per scontato che $B$ sia gia avvenuto.

$$
P(A\mid B)=\frac{P(A\cap B)}{P(B)}
$$

### Esempio

Io ho 2 eventi $A$ = "sono donne" $\frac1 3$, $B$ = "persone alcolizzate" $\frac{1}{10}$.

Prendendo a caso una persona, qual'è la probabilità che sia alcolizzata?

$$
\begin{aligned}
& P(B\cap A)=P(A)\cdot P(B\mid A) & [\text{visto che }A\text{ e }B\text{ sono eventi indipendenti}]\\
& =P(A)\cdot P(B)=\frac{1}{30}
\end{aligned}
$$

Prendendo a caso una donna, qual'è la probabilità che sia alcolizzata?

$$
\begin{aligned}
& P(B\mid A) = \frac{P(B\cap A)}{P(A)}\\
& =\frac{P(B)\cdot P(A)}{P(A)}=P(B)=\frac{1}{10}  & [\text{visto che }A\text{ e }B\text{ sono eventi indipendenti}]
\end{aligned}
$$

Se volessi trovare gli uomini alcolizzati farei: C = "sono uomini" $\frac 2 3$:

$$
P(B\mid C) = \frac{P(B\cap C)}{P(C)}=\frac{P(B)\cdot P(C)}{P(C)}=P(B)=\frac{1}{10}
$$

## Formula della probabilità totale

La formula della probabilità totale dice che $P(A\cup B)=P(A)+P(B)-P(A\cap B)$.

## Formula di bayes

La formula di bayes serve per calcolare $B\mid A$ avendo $A\mid B$:

$$
P(B\mid A) = \frac{P(B)\cdot P(A\mid B)}{\sum\limits_{\forall i} P(A_i)\cdot P(A\mid A_i)}
$$

dove $A_i$ sono tutti gli eventi dipendenti da $A$.
