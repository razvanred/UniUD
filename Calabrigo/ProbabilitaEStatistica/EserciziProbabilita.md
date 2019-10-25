N.B.: Consiglio di vederti gli altri file di probabilità prima di guardare gli esercizi.
# Capitolo 3
Esercizio 3.6 Da un mazzo di carte trevisane, si estrae a caso una carta. Si
calcoli la probabilità che sia o un re o una carta di denari.\
E = "la carta è un re o una carta di denari" quindi |E| = 13 ; |S| = 40\ ; esperimento = "estraggo una carta\
P(E) = |E|/|S| = 13/40\
altro metodo (usando la formula):\
P(E) = ((27 su 0) * (13 su 1)) / (40 su 1) = 13/40

Esercizio 3.7 Allo stadio di San Siro in occasione di una certa partita in tribuna nove persone su dieci sono tifosi o tifose del Milan e sempre nove persone
su dieci guadagnano piu di 50 000 Euro l’anno. Cosa si puo dedurre sulla probabilità che una persona scelta a caso tra quelle in tribuna sia un tifoso/a del
Milan che guadagna almeno 50 000 Euro l’anno?\
E = "tifoso del milan e guadagna più di 50000 euro" ; esperimento = "estrai a caso una persona" ; |S| = 10\
`P(E) = P(E1 intersecato E2) = P(E1) * P(E2) = (9/10) * (9/10) = 81/100`. Con E1 = "tifoso del milan" e E2 = "guadagna più di 50000 euro"\

Esercizio 3.8 Allo stadio di San Siro in occasione di una certa partita in tribuna due persone su dieci sono tifosi/e del Milan e una persona su dieci guadagna
meno di 50 000 Euro l’anno. Cosa si puo dedurre sulla probabilita che una persona scelta a caso tra quelle in tribuna sia o una persona tifosa del Milan oppure
guadagni meno di 50 000 Euro l’anno?\
A = "tifosi del milan", B = "guadagna meno di 50k euro"\
P(A unito B) = P(A) + P(B) - P(A intersecato B) = visto che A e B sono due eventi Indipendenti = 2/10 + 1/10 - (2/10 * 1/10) = 3/10 - 2/100 = 28/100 = 28%\

Esercizio 3.13 Si valuti in quanti modi distinti dieci persone attive in politica
possono essere scelte come ministri/e di cinque ministeri diversi.
D:10,5 = 10!/(10-5)! = 10*9*8*7*6 = 30240\
Perchè ho usato le disposizioni al posto delle combinazioni?

Esercizio 3.17 Un’urna contiene 20 palline nere e 80 palline bianche. Si estraggono in blocco 5 palline. Si calcoli la probabilità di vedere nel campione estratto 3 palline nere (e 2 bianche).\
((20 su 3) * (80 su 2)) / (100 su 5) = (1140 * 3160) / 75287520 = 0.0478 = 4.8%\

Esercizio 3.18 Si lanciano 2n monete equilibrate. Si calcoli la probabilità che
il numero delle teste sia uguale a quello delle croci.
Questo problema si traduce in: In quanti modi, lanciando 2n monete, escono fuori n teste? La soluzione è in (2n su n) modi. Poi, per ottenere la probabilità, divido (2n su n) per il numero totale di modi in cui posso lanciare 2n monete, ovvero 2^(2n). Quindi la soluzione è (2n su n) / 2^(2n). Esempio: se n = 10 (e 2n = 20), allora (20 su 10) / 2^(20) = 0.176 = 18%

# Capitolo 4
Esercizio 4.5 Da un mazzo di carte trevisane si estraggono due carte, lasciandole coperte. Si scopre la prima carta, è un asso (evento A1). Si calcoli la
probabilità che la seconda carta sia un asso. E' una probabilità condizionale?

Probabilità ipergeometriche\
Possiamo rimaneggiare il testo e farlo diventare: Qual'è la probabilità di pescare 2 assi? La probabilità è ((4 su 1) * (4 su 1)) / (40 su 2) = 0.021 = 2.1%. Questa probabilità non è una probabilità condizionale perchè scoprire la prima carta (evento A1) non ha alcun effetto sulla probabilità che anche la seconda carta sia un asso visto che l'ho gia pescata. L'evento A1, e di conseguenza l'evento A2 = "scopre la seconda carta", hanno invece effetto sulle pescate successive.


Formula probabilità composta\
Esercizio 4.8 Siano A, B, C e D eventi e A ∩ B ∩ C sia non trascurabile. Si
scriva la formula della probabilità composta per P(A ∩ B ∩ C ∩ D).\
`P(A ∩ B ∩ C ∩ D) = P(B ∩ C ∩ D) * P(A | B ∩ C ∩ D) = P(C ∩ D) * P(B | C ∩ D) * P(A | B ∩ C ∩ D) =  P(D) * P(C | D) * P(B | C ∩ D) * P(A | B ∩ C ∩ D)`.

Formula probabilità totale\
Esercizio 4.11 Si lancia un dado equilibrato. Il punteggio ottenuto dal dado
determina il numero di lanci indipendenti di una moneta equa: se il punteggio è
pari, la moneta e lanciata due volte; se e dispari, la moneta è lanciata tre volte.
Si calcoli la probabilità di ottenere due teste.

A ="esce un numero pari dal dado" (1/2), B ="esce un numero dispari dal dado" (1/2), C="escono 2 teste da 2 monete"(1/4), D="escono 2 teste su 3 monete"(1/8)\
Io so che C e D sono dipendenti rispettivamente da A e B, però sono indipendenti tra loro. `P(C intersecato D) = P(A) * P(C|A) + P(B) * (D|B) = P(A) * P(A intersecato C)/P(A) + P(B) * P(B intersecato D)/P(B) = P(A) * P(C) + P(B) * P(D) = (1/8) + (1/16) = (3/16)`