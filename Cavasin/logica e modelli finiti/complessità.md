# Complessità

L $\subseteq$ NL $\subseteq$ P $\subseteq$ NP $\subseteq$ PSPACE $=$ NPSPACE $\subseteq$ EXP\
L $\subset$ PSPACE\
P $\subset$ EXP\
STARFREE $\subset$ FO $\subset$ REG $\subset$ MSO
<!-- STARFREE $\subset$ FO -->

## Codifica di strutture relazionali

Siano $I=(D,R_1,\dots,R_m)$ e $\vec a=a_1,\dots,a_h$ in $L$, si definisce $\text{enc}(I,\vec a)$:

* si fissa un ordine arbitrario del dominio $d_1<\dots<d_n$
* si codifica $n$ in unario
* ciascuna relazione $k$-aria è rappresentata da una bitmask lunga $n^k$ sui valori di verità delle $k$-uple, in ordine lessicografico
* $\vec a$ è rappresentato con il suo indice nell'ordine delle $h$-uple

Si ha che $|\text{enc}(I,\vec a)|<p(|I|)$, dove $p$ è un polinomio. Inoltre, fissata una formula atomica $R(\vec x)$ si ha che $(I,\vec a)\models^?R(\vec x)$ è in L e P.

Si definisce allo stesso modo $\text{enc}(I,<)$, con le seguenti proprietà:

* $\text{enc}(I,<)=\text{enc}(J,<)\iff (I,<)\cong(J,<)$
* esistono $\beta(\vec x)$, $\varepsilon(\vec x)$ FO in $L'=L\cup\{<\}$ tali che:
  * $(I,<)\models\beta(\vec a)\iff$l'$\vec a$-esimo simbolo di $\text{enc}(I,<)$ è 1
  * $(I,<)\models\varepsilon(\vec a)\iff\vec a>|\text{enc}(I,<)|$

## FO

| complexity | istanza      | input                     | FO      |
| ---------- | ------------ | ------------------------- | ------- |
| expression | $(I,\vec a)$ | $F(\vec x)$               | PSPACE! |
| data       | $F(\vec x)$  | $(I,\vec a)$              | P       |
| combined   |              | $(I,\vec a)$, $F(\vec x)$ | PSPACE! |

Dato l'input "$\text{enc}(I,\vec a)$#$F$", $F$ va scomposta e il valore di verità delle sottoformule intermedie va memorizzato impiegando spazio al più $|F|$. Serve un contatore $\log(n)$ sul dominio di $I$, spazio $\leq|F|$ per riscrivere la formula istanziata sull'ultimo quantificatore e $S(i+1)$ per procedere ricorsivamente:

$$
S(i)=|F|+\log(n)+S(i+1)
$$

Si può dimostrare che lo spazio totale è $O(\vert F\vert^2+\vert F\vert\log(\vert I,\vec a\vert))$

**Completezza**: Dimostrata con TQBF $\preceq$ FO. La riduzione non dipende da $(I,\vec a)$

**Data Complexity**: Fissato $F$, il problema è L, più precisamente si può dimostrare che è P

## Fixed Parameter Tractability

L'istanza di un problema parametrizzato si divide in una parte di grandezza $n$ e un parametro $k$. Un problema parametrizzato si dice fixed parameter tractable (FPT) se può essere risolto in tempo $f(k)*p(n)$, dove $f$ è computabile e $p$ è un polinomio. Si assume che $k$ sia piccolo

**FPT Model Checking**: Data $L$ e una classe di strutture $C$, model checking di $L$ su $C$ è FPT se per ogni $I\in C$ e $F\in L$ si ha che $I\models^?F$ è $O(f(|F|)+|I|^m)$

### FO su strutture di grado limitato

Dimostrato con località TODO

## Complessità Descrittiva

Complessità descrittiva unisce complessità a FMT per caratterizzare le classi di complessità in base al tipo di logica richiesta per esprimere i linguaggi (classi su strutture finite) che contengono.

Si nota che una logica $L$ con qualche data complexity C! non implica che $L$ possa descriverne tutti i linguaggi, ma solo che esiste una riduzione tra strutture.\
Inoltre, per ogni classe C catturata dalla logica $L$ si ha che complessità descrittiva C $\subseteq$ data complexity $L$

### Fagin: $\boldsymbol\exists$SO cattura NP

$C$: strutture finite

Per ogni classe $C$ decisa da una MDT in NP esiste $F_C$ in tale che $I\in C\iff I\models F_C$.

Sia $M_C=\langle Q,\{0,1,\#\},q_0,Q_a,Q_r\rangle$. Scegliamo un $k$ tale che ogni computazione su $(I,<)$ termini in tempo e spazio limitati da ${|I|}^k$.

Si definisce $F_C=\exists{<}\exists T_0\exists T_1\exists T_\#\exists H_{q_0}\dots\exists H_{q_n}F_M$, dove le relazioni hanno arietà $k+k$:

* $T_0$, $T_1$, $T_\#$: Predicati del nastro, $T_i(\vec p,\vec t)$ indica che al tempo $\vec t$ la posizione $\vec p$ contiene il simbolo $i$
* $\{H_q\mid q\in Q\}$: Predicati della testina, $H_q(\vec p,\vec t)$ indica che al tempo $\vec t$ la macchina è nello stato $q$ in posizione $\vec p$

$F_M$ FO esprime l'esistenza di una computazione accettante per $\text{enc}(I,<)$ in meno di ${|I|}^k$ passi. È composta dalla congiunzione dei seguenti enunciati:

* $<$ è un ordine lineare
* in ogni momento ogni cella del nastro contiene esattamente un elemento di $\Sigma$
* in ogni momento $M_C$ è in esattamente uno stato
* $T$ e $H$ rispettano le transizioni di $M_C$
* la configurazione iniziale con l'input sul nastro:

  $$
  H_{q_0}(\vec0,\vec0)\land\forall\vec p\left(T_1(\vec0,\vec p)\leftrightarrow\beta(\vec p)\land(T_\#(\vec 0,\vec p)\leftrightarrow\varepsilon(\vec p))\right)
  $$

**Osservazione**: Model checking di $F_C$ $\exists$SO simula satisfiability FO $F_M$ (come per Trakhtenbrot) con un limite combinatorio sul dominio dei predicati di computazione $T$ e $H$

**Data Complexity**: Guess and verify. Sia $\exists R_1\dots\exists R_k F$ con $F$ FO, usando il non-determinismo possiamo indovinare $R_1,\dots,R_k$ e verificare $I\cup {R_1,\dots,R_k}\models F$ in P

**$\boldsymbol\forall$SO cattura coNP**

Per dimostrare P≠NP è sufficiente esibire una proprietà dei modelli finiti esprimibile in $\exists$SO ma non $\forall$SO. Nella classe di tutte le strutture la separazione è nota

### Büchi: MSO($\lt$) cattura REG

$L$: finito con $\lt$\
$C$: ordini lineari finiti

Sia REG la classe si complessità dei linguaggi regolari. In questo caso la restrizione è strutturale sulla MDT che riconosce il linguaggio: deve essere un automa a stati finiti, ovvero può solo leggere il nastro di input e muovere la testina verso destra.

**Teorema**: Un linguaggio $L$ è REG $\iff\big[$esiste $F_L$ tale che $I\in L\iff I\models F_L\big]$\
$\implies$: Dimostrato codificando il DFA in MSO, si quantificano esistenzialmente $P_1,\dots P_n$\
$\impliedby$: Si costruisce un DFA sull'alfabeto $\{0,1\}^n$ dove ciascuna parola codifica un ordine lineare finito etichettato con $P_1,\dots P_n$. Dimostrato passando attraverso MSO<sub>0</sub>, dove sono permesse solo variabili al second'ordine, e definendo una traduzione in DFA sulla struttura della formula

**Teorema**:
SAT<sub>F</sub> MSO nella classe degli **ordini lineari finiti** è R\
La proprietà del linguaggio non vuoto è decidibile per gli automi finiti

### Star-Height: FO($\lt$) cattura STARFREE

$L$: finito con $\lt$\
$C$: ordini lineari finiti

**Teorema**: Un linguaggio $L$ è STARFREE $\iff\big[$esiste $F_L$ tale che $I\in L\iff I\models F_L\big]$\
Dimostrato in maniera complicata usando relativizzazioni

**Teorema**: $L$ è star-free$\iff$esiste $\mathcal A_L$ counter-free\
Un automa è counter-free se non ha parole che definiscono cicli non-triviali. Una parola $\sigma\in\Sigma^*$ definisce un ciclo non-triviale se esiste uno stato $q\in Q_\mathcal A$ dove $\delta^*(q,\sigma)\neq q$ e $\delta^*(q,\sigma^n)=q$ per qualche $i>1$. Un ciclo non-triviale è un contatore di $\sigma$ modulo $n$

**Corollario**: Determinare se $\text{L}(\mathcal A)$ è STARFREE è PSPACE!

### Immerman-Vardi: LFP($<$) cattura P

$L$: con $<$ e minimo\
$C$: ordini lineari finiti

Per ogni classe $C$ negli ordini lineari finiti decisa da una MDT in P, esiste $F_C$ in tale che $I\in C\iff I\models F_C$.

Dimostrato seguendo lo schema di Fagin. Le formule sono scritte in modo che la $k+1$-esima iterazione del sistema rappresenta le configurazioni fino al tempo $k$, limitate polinomialmente dal dominio. Le formule non sono positive e si sfrutta quindi LFP=IFP

**Data Complexity**: Ogni sottoformula FO è verificabile in P. L'iterazione di punto fisso si ferma entro $|I|^k$ passi.

**Teorema**: LFP=LFP<sup>sim</sup>=IFP=IFP<sup>sim</sup>

**Osservazione**: **se** $G$ è monotono IFP$(G)$ $=$ LFP$(G)$. Il minimo punto fisso contiene comunque l'input

PFP cattura PSPACE\
Partial Fixed Point

P=PSPACE$\iff$LFP=PFP
