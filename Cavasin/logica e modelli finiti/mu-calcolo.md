# $\mu$-Calcolo

**Labelled Transition Systems**: $\langle W,w,R,P_1,P_2,\dots\rangle$, modelli composto da stati $W$ (anche infiniti), uno stato radice $w$, e transizioni $R$. Gli stati sono etichettati con letterali $P_1$, $P_2$, $\dots$

## Bisimulazione

Siano due LTS $I$ e $J$. Una **relazione** $B\subseteq I\times J$ è una bisimulazione tra $I$ e $J$ se $B(w_I,w_J)$, e per ogni $B(a,b)$ si ha:

* $I,a\models P_i\iff J,b\models P_i$
* **forth**: per ogni $R(a,a')$ esiste $B(a',b')$ tale che $R(b,b')$
* **back**: per ogni $R(b,b')$ esiste $B(a',b')$ tale che $R(a,a')$

Se $I$, $J$ sono bisimili scriviamo $I\sim J$. La bisimulazione è una relazione d'equivalenza fra LTS più debole dell'isomorfismo.

Sia $C$ una classe di LTS. $C$ è invariante per bisimulazione se $I\in C\iff J\in C$. Le proprietà invarianti per bisimulazione sono proprietà del sistema e non della rappresentazione

## Logica Modale LM

Le formule modali aggiungono alle formule proposizionali su $P_1$, $P_2$, $\dots$ due operatori unari $\Box F$ e $\Diamond F$, interpretati rispettivamente come "è necessario che valga $F$" ed "è possibile che valga $F$"

### LM $\subset$ FO(x)

Sia $\text{ST}\colon$ ML $\mapsto$ FO(X) la formula che traduce formule ML in formule FO con una variabile libera:

$$
\begin{align*}
I\models\Box F&&\forall a\in W\ R(w,a)\rightarrow\text{ST}(F)[w/a]\\
J\models\Diamond F&&\exists a\in W\ R(w,a)\land\text{ST}(F)[w/a]
\end{align*}
$$

Quindi per località le formule ML riescono ad esprimere solo proprietà locali di un LTS

**Teorema**: ML corrisponde esattamente alle formule FO(x) invarianti per bisimulazione

### Giochi Modali

* arena costituita da un LTS e $F$ in una data logica, la cui verità dipende dallo stato dell'LTS in cui viene valutata
* due giocatori, spoiler e duplicator
* una posizione del gioco è $(a,G)$ dove $a\in W$ e $G$ è una sottoformula di $F$. La posizione iniziale è $(w,F)$

I giochi modali estendono i giochi proposizionali con $\Box F$, dove gioca spoiler, e $\Diamond F$, dove gioca duplicator. Come per i giochi proposizionali, sono giochi finiti

**Teorema**: $I,a\models F\iff$ duplicator nel gioco $(a,F)$

## $\mu$-calcolo

LM $\subseteq\mu\subseteq$ MSO(X)\
$\mu$-calcolo è un'estensione della logica modale, e un frammento di MSO(X)

**Teorema**: Il $\mu$-calcolo corrisponde esattamente alle formule MSO(X) invarianti per bisimulazione\
Dimostrato implementando le definizioni di punto fisso di Tarski:

$$
I,a\models\mu XF(X)\iff I\models\forall X(X\subseteq F(X)\rightarrow X(a))\\[.5em]
I,a\models\nu XF(X)\iff I\models\exists X(F(X)\subseteq X\land X(a))
$$

Le formule su cui sono applicati gli operatori di punto fisso $\mu X\,F(X)$ e $\nu X\,F(X)$ devono essere monotone su $X$ per poter usare l'iterazione funzionale. Ciò equivale a chiedere che $\neg$-height di ogni occorrenza di $X$ in $F$ sia pari

### Giochi $\mu$-calcolo

I giochi $\mu$-calcolo estendono i giochi modali con gli operatori di punto fisso. Le posizioni $(w,F)$ con $F=\mu X\,G(X)$ o $\nu X\,G(X)$ portano ad un altro turno (riciclo) per il giocatore corrente alla posizione $(w,G(F))$. Le partite possono quindi essere infinite

**Teorema**: $w\models F\iff$duplicator nel gioco $(w,F)$

* su $\mu X\,G(X)$ con $G$ modale, spoiler vince tutte le partite infinite\
  L'iterazione funzionale richiede l'esistenza di un caso base modale in $G$
* su $\nu X\,G(X)$ con $G$ modale, duplicator vince tutte le partite infinite\
  Duplicator non riesce a trovare un controesempio per la condizione modale in $G$
* su punti fissi innestati come $\nu X\mu Y\,G$, duplicator vince riciclando infinite volte $\nu$ ma, ad ogni ciclo, finite volte $\mu$. Si numerano gli operatori innestati con $\nu$ pari e $\mu$ dispari:

   $$
   \nu^4X\mu^3Y\nu^2Z\mu^1u\,G
   $$

   Si ottiene un gioco di parità: in una strategia di duplicator $\mu^3Y$ può essere riciclato infinite volte solo se anche $\nu^4X$. Il vincitore è determinato dalla parità del massimo indice visto infinite volte
