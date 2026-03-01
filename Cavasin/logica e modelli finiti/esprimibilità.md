# Esprimibilità FO

**Lemma 1**: Una proprietà $P\subseteq C$ in una classe $C$ di in un linguaggio relazionale **finito** si dice esprimibile FO su $C$ in rango $n$ se esiste $F$ FO con $rk(F)\leq n$ tale che $\forall I\in C\quad I\models F\iff I\in P$

**Lemma 2**: In $L$ **finito**, fissate le variabili $x_1,\dots,x_k$ e a meno di conseguenza logica esistono un numero finito di formule di rango $\leq n$ con variabili libere $x_1,\dots,x_k$.\
Esiste quindi una $F$ tale che $J\models F(x_1,\dots,x_k)\iff I,a_1,\dots,a_k\equiv_nJ,b_1,\dots,b_k$

## Compattezza

$C$: tutte le strutture

$\Gamma$ è soddisfacibile $\iff$ ogni $\Delta\subseteq\Gamma$ finito è soddisfacibile\
$\implies$ per definizione di soddisfacibilità di una classe di formule\
$\impliedby$ dimostrato sfruttando il fatto che esiste una computazione finita che dimostra $\Delta\vdash\bot$

**Strutture Finite**: Sia $\Gamma=\{x_i\neq x_j\colon i,j\in\mathbb N,\ i\neq j\}$. Nella classe delle strutture finite $\Gamma$ è insoddisfacibile, ma ogni sottoinsieme finito ha un modello finito.

## Giochi EF

$C$: tutte le strutture

* in generale:

  $I\not\equiv_nJ\implies$ spoiler in $EF_n(I,J)$\
  $I\equiv_nJ\impliedby$ duplicator in $EF_n(I,J)$

  Dimostrato descrivendo la strategia di spoiler induttivamente sulla struttura di $F$
* $L$ **finito**:

  $I\equiv_nJ\iff$ duplicator in $EF_n(I,J)$

  Dimostrato descrivendo la strategia di duplicator induttivamente sulla struttura di $F$ del *lemma 2*

  **Controesempio**: Sia $L=\{P_1,P_2,\dots\}$ infinito. Qualsiasi formula finita può menzionare solo un numero finito di relazioni, mentre spoiler verifica l'isomorfismo e può distinguere, per esempio, un nodo su cui valgono infiniti $P_i$
  * $I$, $J$ numerabili:

    $I\cong J\impliedby$ duplicator in $EF_\omega(I,J)$\
    $I\equiv_\omega J\iff I\cong J$

    Dimostrato descrivendo la strategia infinita di spoiler enumerando in ordine gli elementi di $I$, $J$ in maniera alternata

    **Controesempio**: Ogni ordine lineare denso senza massimo e minimo è elementarmente equivalente, ma $\mathbb R\not\cong\mathbb Q$. La teoria corrispondente è $\aleph_0$-categorica

**Teorema**: Siano $I^k$, $J^m$ due ordini lineari finiti di cardinalità $k$ e $m$. Si ha $k,m\geq2^n-1\implies$duplicator in $EF_n(I^k,J^m)$\
Dimostrato osservando che duplicator può mantenere distanze sufficienti tra i nodi per coprire ogni strategia

## Random Graph $rg$

$L$: finito senza costanti\
$C$: strutture finite

**0-1 Law**: Per ogni F in $L$ finito senza costanti si ha $\mu(F)\in\{0,1\}$\
Se esiste $|I|\geq2n$ tale che $I\models EA_n\land F$, esistono infiniti arbitrariamente grandi $J\models EA_n$ tali che $I\equiv_nJ$, quindi $\mu(EA_n)\leq\mu(F)$.\
Altrimenti, ogni $I\models EA_n\land\neg F$, e $\mu(EA_n)\leq\mu(\neg F)$

**Teorema 1**: Per ogni $F$ si ha $\mu(F)=1\iff rg\models F$\
$\impliedby$: Dal momento che $|rg|\geq2n$ e $rg\models EA_n\land F$, si applica la 0-1 law\
$\implies$: Da sopra, si ha $\mu(F)\neq1\implies rg\not\models F$. Quindi per completezza $rg\models\neg F$, $\mu(\neg F)=1$, e $\mu(F)=0$. Assurdo

**Teorema 2**: $rg\models F$, $\mu(F)=1$ sono decidibili\
$T_{rg}$ è completa, quindi rispondiamo `si` se $T_{rg}\vdash F$, `no` se $T_{rg}\vdash\neg F$

$T_{rg}$ in $L=\{R\}$ comprende i seguenti assiomi:

1. $\exists x\exists y\ x\neq y$
2. $\forall x\ \neg R(x,x)$
3. $\forall x\forall y\ R(x,y)\rightarrow R(y,x)$
4. $\forall n\ EA_n$

Proprietà:

* ogni $I\models T_{rg}$ è infinita e numerabile
* $T_{rg}$ è coerente

  Dimostrato con un indice-bitmask delle connessioni per nodo
* $T_{rg}$ è $\aleph_0$-categorica. $I\models T_{rg}$ si chiama $rg$

  Per ogni $I$, $J$ si ha $I\equiv_nJ$. Essendo numerabili si ha $I\cong J$
* $T_{rg}$ è completa

  Dimostrato con criterio di Vaught
* $T_{rg}$ è consistente

### Assioma di Estensione

**Lemma 1**: Dati due grafi $I,J\models EA_n$ con $|I|,|J|\gt2n$, si ha $I\equiv_nJ$\
Dimostrato descrivendo la strategia di duplicator al turno $h<n$, scegliendo $z$ di $EA_h$

$$
EA_{n,m}=\forall x_1,\dots,\forall x_n\left(\bigwedge_{1\leq i<j\leq n}x_i\neq x_j\rightarrow\exists z\left(\bigwedge_{1\leq i\leq n}x_i\neq z\land\bigwedge_{1\leq i\leq m}\neg R(x_i,z)\land\bigwedge_{m<i\leq n}R(x_i,z)\right)\right)\\[1em]
EA_n=EA_{2n,n}
$$

* per ogni $n\geq0$ si ha $\mu(EA_n)=1$

  Dimostrato combinatorialmente
* ogni $EA_n$ ha modelli finiti arbitrariamente grandi

  Dimostrato come conseguenza di $\mu(EA_n)=1$
* se $|I|\geq2n$ e $I\models EA_n$ si ha $\forall_{i\leq n}I\models EA_{i}$ e $\forall_{m\leq n}I\models EA_{n,m}$

  Dimostrato come restrizione della formula. Intuitivamente $i,m\leq n$
* per ogni $m\leq n$ si ha $\mu(EA_{n,m})=1$

  $\mu(EA_n)\leq\mu(EA_{n,m})$

## Località

$C$: tutte le strutture\
$L$: finito

**Hanf-equivalenza**: Siano $I$, $J$. Si scrive $I\leftrightarrows_rJ$ quando per tutti gli $x\in I$ e $x\in J$ vale:

$$
|\{a\mid\mathcal N_r(a)\cong\mathcal N_r(x)\}|=|\{b\mid\mathcal N_r(b)\cong\mathcal N_r(x)\}|
$$

In altre parole, $I\leftrightarrows_rJ$ quando per ogni **tipo** $\tau$ di intorni, $I$ e $J$ hanno lo stesso numero di isomorfismi che realizzano $\tau$

**Teorema**: Siano $(I,\vec a)$ e $(J,\vec b)$ tali che, per un certo $m$:

$$
r=\frac{3^m-1}2\\[.5em]
I\leftrightarrows_rJ\\[.4em]
(\mathcal N_r(\vec a),\vec a)\cong(\mathcal N_r(\vec b),\vec b)
$$

Si ha $(I,\vec a)\equiv_m(J,\vec b)$.

Mostrando che esistono infiniti $I\in P$, $J\notin P$ Hanf-equivalenti, si dimostra che $P$ non è esprimibile FO su $C$
