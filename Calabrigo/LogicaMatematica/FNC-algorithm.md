# FNC e FND
Teo: Una qualsiasi formula F può essere trasformata in una funzione in Forma Normale Congiuntiva (G1) e/o in una funzione in Forma Normale Disgiuntiva (G2).\
\
Una formula in FNC è composta da un insieme di sottoformule H1,H2,...,Hn messe tutte in congiunzione (and) tra loro. Ogni formula Hi, invece, è formata da lettere proposizionali (tipo variabili booleane) messe tutte in disgiunzione tra loro.

Es.: F1 and F2  dove F1 = (p or t) e F2 = (p or n)\
quindi ((p or t) and (p or n)) = K, dove K è in FNC\
Scambiando gli or con gli and ottengo una formula in Forma Normale Disgiuntiva.\
((p and t) or (p and n)) = S\
N.B.: le formule K e S non sono le stesse, una volta ottenuta K non basta scambiare and e or per ottenere S.

# Algoritmo di FNC e FND

L'algoritmo serve a trasformare una qualunque funzione F in una funzione K in FNC o FND. Lo scopo è semplificare lo studio della formula e avere certi vantaggi. Per esempio in una sottofunzione di una funzione FND sappiamo che se ci sono due lettere proposizionali complementari, quella formula darà sempre falso.\
Es.: ((p and not(p) and q and z and not(i) and j) or (...)), sappiamo che la prima sottoformula è falsa.\
\
L'algoritmo (per FNC) esegue un numero di passi sulla funzione in input F, e smette con K uguale alla FNC di F, altrimenti eseguo l'algoritmo su la sua prima sottoformula G. In questo caso so che G non è un letterale, quindi posso procedere in questi modi:
* se G è una doppia negazione, allora rimpiazzo G con il suo ridotto (se G = not(not(N))), allora rimpiazzo G con N
* se G è una beta-formula, rimpiazzo G con i suoi ridotti
* se G è una alpha-formula, faccio cose brutte, ovvero uso la formula del caro DeMorgan: F or (G and H) = ((F or G) and (F or H)), poi vediamo cosa significa in un esempio.

Al posto delle virgole possiamo sostituire and per le alpha-formule e or per le beta-formule, e i conti tornano. Intanto vediamoci le tabelle di alpha-formule:\
| alpha-formule | ridotti       |
|:-------------:|:-------------:|
| F and G       | F,G           |
| not(F or G)   | not(F), not(G)|
| not(F->G)     | F , not(G)    |
E poi le beta-formule:
| beta-formule | ridotti       |
|:-------------:|:-------------:|
| F or G        | F,G           |
| not(F and G)  | not(F), not(G)|
| F->G          | not(F) , G    |


Introduciamo una nuova notazione per l'algoritmo, sostituiamo gli or con [,] e gli and con <,>.:\
((p or t) and (p or n)) diventa <[p,t],[p,n]>\

Questo è l'algoritmo, ma ora vediamo degli esempi: Trasformiamo F in FNC:\

INIZIO\
F = `((r and not(s)) or not(p->q))`\
Per prima cosa cambiamo la sintassi della funzione per usare l'algoritmo, qui potrei decidere di trovare <[..]>, e in questo caso troverei la FNC, oppure [<..>], per trovare la FND. Noi scegliamo la prima:\
`<[((r and not(s)) or not(p->q))]>`\
Abbiamo due grandi sottoformule: G = (r and not(s)) e H = not(p->q). G e H sono messe in or, questo corrisponde alla prima beta-formula, quindi riscriviamo i ridotti:\
`<[((r and not(s)) , not(p->q))]>`\
Ora guardiamo la sottoformula a G = ((r and not(s)). Corrisponde alla prima alpha-formula F and G dove F = r e G = not(s). Ora viene un passaggio bastardo, perchè non basta sostituire i ridotti ma bisogna usare la formula di DeMorgan sovracitata (F or (G and H) = ((F or G) and (F or H))), dove F = not(p->q), G = r, e H = not(s). Quindi riscriviamo:\
`<[not(p->q),r],[not(p->q),not(s)]>`\
Rimaniamo un attimo su questo passaggio, perchè lo abbiamo fatto e non abbiamo solo scritto i ridotti? Beh noi abbiamo <[((r and not(s)) , not(p->q))]>, utilizzando gli assegnamenti per F,G e H di prima avremmo: <[(G and H) , F]>, possiamo ignorare l'and <>, e quindi, se sostituiamo le [,] otteniamo: (G and H) or F. Ora se sotituissimo (G and H) con i ridotti nella alpha-formula otterrei (G, H) or F. Ora sostituiamo i valori a G e H e rimettiamo le parentesi: (G, H) or F diventa (r or not(s)) or not(p->q), che diventa <[(r or not(s)) , not(p->q)]>. Abbiamo ottenuto una formula sbagliata perchè <[(r or not(s)) , not(p->q)]> è diversa da <[(r and not(s)) , not(p->q)]>. Questo dimostra che se sto cercando una FNC, allora quando applico le alpha-formule devo usare anche la formula di De Morgan, se sto cercando le FND, invece, funziona al contrario (alpha-formule solo ridotti, e beta-formule ridotti + De Morgan).\
A questo punto possiamo continuare, abbiamo una alpha-formula [not(p->q)] or r, ridotta diventa (p and not(q)) or r, e poi applichiamo DeMorgan:\
`<[p,r],[not(q),r],[not(p->q),not(s)]>`\
A questo punto abbiamo la stessa alpha-formula, con not(p->q) or not(s) che diventa (p and not(q)) or not(s):\
`<[p,r],[not(q),r],[p,not(s)],[not(q),not(s)]>`\
Bene abbiamo ottenuto un K che sia in FNC, dove ogni disgiunto di ogni congiunto è un letterale. Ora togliamo la sintassi scomoda e abbiamo finito:\
K = `((p or r) and (not(q) or r) and (p or not(s)) and (not(q) or not(s)))`\
FINITO