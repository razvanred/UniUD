# Extra

## Los-Tarski

$C$: tutte le strutture

$F(x_1,\dots,x_k)$ FO è chiusa per sottostrutture $\iff$ è equivalente ad una formula universale

**sottostruttura**: generalizzazione di sottografo indotto

$\impliedby$: Dimostrato semanticamente

$\implies$: Sia $\Phi=\{\phi\mid F\models\phi$, $\phi$ è universale$\}$

* dimostrando $\Phi\models F$ si ha $\Phi\equiv F$. Sia $I\models\Phi$:
  * dimostrando che esiste $J\models F$ con $J'\subseteq J$ tale che $J'\cong I$, si ha $I\models F$ perché $F$ è chiusa per sottostrutture
    * consideriamo un insieme di costanti $C=\{c_d\mid d\in D^I\}$ non utilizzato nelle formule in $\Phi$ e $F$ e la segnatura $L'=L\cup C$:

      $$
      diag(I)=\{\alpha(c_{d_1},\dots,c_{d_k})\mid I\models\alpha(d_1,\dots,d_k),\ d_1,\dots,d_k\in D^I\}
      $$

      Con $J\models diag(I)$ si ha che $J$ contiene $J'$.

      Se non esiste $J\models diag(I)\cup \{F\}$, per compattezza esiste $\Delta\subset diag(I)$ finito dove $F\models\neg\alpha(c_{d_1},\dots,c_{d_k})$, ma in tal caso $F\models\forall x_1\dots\forall x_k\neg\alpha(x_1,\dots,x_k)$ poichè $F$ è in $L$.\
      La formula universale consegue da $F$ quindi è in $\Phi$. Ma $I\models\Phi$ e $I\models\alpha(c_{d_1},\dots,c_{d_k})$ per definizione di diagramma, assurdo
* dimostrando che esiste $\phi\in\Phi$ universale tale che $\phi\models F$ si ha $\phi\equiv F$
  * dato che $\Phi\models F$ per compattezza esiste $\Delta\subseteq\ \Phi\cup\{\neg F\}$ finito insoddisfacibile

**Osservazione**: Per ogni $I\models\Phi$ esiste sempre un $J\models F$ con $J'\subseteq J$ tale che $J'\cong I$. Intuitivamente, espandiamo $I$ quanto basta affinché $J\models F$ e poi discendiamo sfruttando la chiusura per sottostrutture

## Teorie

Una teoria $T$ si dice:

* **coerente**: se esiste un modello $I\models T$
* **completa**: se per ogni $F$ si ha $T\vdash F\ \lor\ T\vdash\neg F$
* **consistente**: $T$ coerente e completa. Tutti i modelli sono elementarmente equivalenti $I\models T\land F\iff T\vdash F$.\
Intuitivamente verificano tutti gli stessi enunciati, le formule conseguenti da $T$. Non può derivare contraddizioni
* **$\boldsymbol{\aleph_0}$-categorica**: se tutti i modelli di cardinalità numerabile sono isomorfi

**Criterio di Vaught**: Ogni $T$ $\aleph_0$-categorica in $L$ **numerabile** con solo modelli infiniti è completa

## Sistemi Back and Forth

**back and forth**: Siano $I$, $J$. Sia $\text{parz}(I,J)$ l'insieme di tutti gli isomorfismi parziali tra $I$ e $J$, ovvero biiezioni $\vec a\subseteq I\mapsto\vec b\subseteq J$. Siano $X\subseteq Z\subseteq\text{parz}(I,J)$. Un isomorfismo parziale $f\in X$ si dice che ha la proprietà di back and forth su $Z$ se valgono:

* **forth**: $\forall a\in I\ \exists b\in J\quad(f\cup a\mapsto b)\in Z$
* **back**: $\forall b\in I\ \exists a\in J\quad(f\cup a\mapsto b)\in Z$

In altre parole, per ogni scelta $a\in I$ o $b\in J$ esiste un isomorfismo con $a\mapsto b$ in $Z$.

Siano $I$, $J$. Un sistema $X_1,\dots,X_m$ si dice back and forth per $(\vec a,\vec b)$ se $\vec a\mapsto\vec b\in X_m$ e ciascun $X_i$ ha le seguenti proprietà:

* $X_i\neq \emptyset$
* $X_i\subseteq\text{parz}(I,J)$
* ogni $f\in X_{i+1}$ è back and forth su $X_i$

Se esiste un sistema back and forth per $(\vec a,\vec b)$ scriviamo $(I,\vec a)\cong_m(J,\vec b)$

**Lemma**: $(I,\vec a)\cong_m(J,\vec b)\iff$ duplicator in $EF_m((I,\vec a),(J,\vec b))\iff(I,\vec a)\equiv_m(J,\vec b)$\
Dimostrato descrivendo la strategia di duplicator induttivamente sulla definizione di back and forth
