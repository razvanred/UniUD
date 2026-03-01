# Decidibilità FO

## Church: VAL è RE!

$L$: numerabile\
$C$: tutte le strutture

Dimostrato con PCP $\preceq$ VAL,\
oppure enumerando gli alberi di deduzione $\top\vdash F$

**SAT è coRE!**

**Teorema 1**: HALT $\preceq$ MPCP $\preceq$ PCP

## Trakhtenbrot: SAT<sub>F</sub> è RE!

$L$: finito con $<$ e minimo\
$C$: strutture finite

Dimostrato con HALT $\preceq$ SAT<sub>F</sub>.

Sia $M=\langle Q,\{0,1,\#\},q_0,Q_a,Q_r\rangle$, si definisce $L=\{{<},0\}\cup\{T_\sigma\mid\sigma\in\Sigma\}\cup\{H_q\mid q\in Q\}$:

* $T_\sigma$: Predicati del nastro, $T_\sigma(p,t)$ indica che al tempo $t$ la posizione $p$ contiene il simbolo $\sigma$
* $H_q$: Predicati della testina, $H_q(p,t)$ indica che al tempo $t$ $M$ è nello stato $q$ in posizione $p$

Sia $F_M$ una formula che esprime l'esistenza di una computazione terminante di $M$, composta dalla congiunzione dei seguenti enunciati:

* $<$ è un ordine lineare con minimo 0
* in ogni momento ogni cella del nastro contiene esattamente un elemento di $\Sigma$
* in ogni momento la macchina è esattamente in uno stato
* $T$ e $H$ rispettano le transizioni di $M$
* la configurazione iniziale
* $M$ si ferma:

  $$
  \exists t\exists p\bigvee_{q\in Q_a\cup Q_r}H_q(p,t)
  $$

$M$ termina sull'input vuoto$\iff$SAT<sub>F</sub>$(F)$

**VAL<sub>F</sub> è coRE!**: SAT<sub>F</sub> è RE!, quindi VAL<sub>F</sub> non è RE
