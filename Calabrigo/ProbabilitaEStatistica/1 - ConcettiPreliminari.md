# Concetti Preliminari

## Eventi incompatibili

Due eventi $A$ e $B$ sono incompatibili se la loro intersezione è l'insieme vuoto. Ovvero se $A$ e $B$ non hanno elementi in comune. Se hanno almeno un elemento in comune, allora sono compatibili.

## Eventi trascurabili

Un evento $A$ è detto trascurabile se $P(A)=0$. Altrimenti è detto non trascurabile.

## Eventi indipendenti

Due eventi $A$ e $B$ sono indipendenti, quando $P(A)\cdot P(B\mid A)=P(A\cap B)=P(A)\cdot P(B)$.\
In questo caso $P(B\mid A)=\frac{P(A\cap B)}{P(A)}=\frac{P(A)\cdot P(B)}{P(A)}=P(B)$, quindi $P(A)\cdot P(B\mid A)=P(A)\cdot P(B)$.

In breve, posso dire che due eventi sono indipendenti quando la realizzazione di uno non influenza in alcun modo la realizzazione dell'altro, facciamo due esempi di eventi dipendenti e indipendenti:

* ### Dipendente:
  
  Una lotteria: Evento $A$="estraggo un biglietto e assegno il primo premio", $B$="estraggo un biglietto e assegno il secondo premio". La realizzazione di $A$ condiziona la realizzazione di $B$, perché dopo aver estratto e assegnato il primo premio, ci sarà una persona in meno che partecipa alla lotteria per il secondo premio (quello che ha vinto il primo premio), e tutti gli altri avranno possibilità aumentate, quindi $P(B\mid A)>P(B)$.
  
  > potrebbe accadere anche che $P(B\mid A)<P(B)$, sarebbe questo il caso se invece di togliere un biglietto, ne avessimo aggiunto uno.

* ### Indipendente:
  
  Lancio di un dado e una moneta: $A$="lancio un dado", $B$="lancio una moneta". Il risultato del lancio del dado non cambia in alcun modo il risultato del lancio della moneta. 
  
  > $P(B\mid A)=P(B)$ (vedi sotto)

Possiamo generalizzare la formula per il calcolo dell'intersezione è sempre:

$$
P(A\cap B)=P(B)\cdot P(A\mid B)
$$

Però, se $A$ e $B$ sono eventi dipendenti, allora $P(A\mid B)=\frac{P(A\cap B)}{P(B)}$, altrimenti $P(A\mid B)=P(A)$.

> Nel caso tu abbia un po' di confusione relativamente alle formule: $P(A\cap B)=P(B)\cdot P(A\mid B)=P(A)\cdot P(B\mid A)=P(B\cap A)$.

## Partizioni di uno Spazio Campionario e Classe degli Eventi

Innanzitutto, cos'è uno spazio campionario? E' l'insieme di tutti i possibili risultati degli eventi elementari. Se tiro 2 monete, il mio spazio campionario sarà $S = \{(T,T),(T,C),(C,T),(C,C)\}$, e gli eventi elementari sono 4: $(T,T)$ $(T,C)$ $(C,T)$ $(C,C)$.\
La classe degli eventi, invece, è l'insieme di tutti gli eventi che utilizzano uno spazio campionario. Nel lancio delle 2 monete, supponendo di avere solo 2 eventi (più quelli elementari) avrei classe degli eventi $F$=$\{(T,T),(T,C),(C,T),(C,C),[(C,T),(C,C)],[(T,T)]\}$, con $E_1=[(C,T),(C,C)]$ e $E_2=[(T,T)]$. In realtà la classe degli eventi contiene tutti gli eventi che ha senso probabilizzare, quindi avrebbe più un aspetto simile: $F=\{(T,T),(T,C),(C,T),(C,C),[(C,T),(C,C)], [(T,T)], \ldots\}$.

> $S$ è contenuto in $F$ e $F$ è contenuto nell'insieme delle parti di $S$.

Ora che abbiamo chiaro il concetto di spazio campionario, cosa sono le partizioni dello spazio campionario? Quando ho una serie di eventi $A_i$, e l'intersezione tra qualunque $A_i$ e un qualunque altro evento è l'insieme vuoto. Praticamente posso vedere $S$ come una tavola, divisa in $n$ parti da dei righelli.
