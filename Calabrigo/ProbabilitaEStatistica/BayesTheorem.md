# Cos'è il teorema di Bayes
Il teorema di bayes è un modo di capire quanto sia probabile un evento condizionale A|B, a patto di avere informazioni sulle probabilità di B|A, A e B.\
Il teorema di Bayes è il seguente:\
`P(A|B) = (P(A) * P(B|A)) / P(B)`\
dove possiamo scrivere P(B), nel caso non lo conoscessimo, come sommatoria per ogni i che appartiene ad I di P(Ai) * P(B|Ai). Quindi posso riscrivere la formula come:\
`P(A|B) = (P(A) * P(B|A)) / sum(P(Ai) * P(B|Ai))`\
Ora che abbiamo esposto le formule, possiamo provare a capirlo. Sarà utile vedere gli esempi per avere più chiaro il concetto, ma praticamente se noi vogliamo sapere quanto spesso accade A|B, noi dividiamo le volte che accadono sia A che B per la somma delle volte che accadono A e B, A e C , ... , A e Z. Quindi cerco: A|B = A intersecato B / (A intersecato B + A intersecato C + ... + A intersecato Z).\
Facciamo un esempio per chiarire il concetto, e l'importanza di bayes. Pensandola in questo modo torno alla formula per trovare P(A|B) = P(B|A) * P(A) / P(B) = P(B intersecato A) / P(B); per capire bayes conviene ragionare su questa formula, ma ora andiamo agli esempi.
## ESEMPIO 1 (con dati del libro)
Esempio 5.1 Un esame diagnostico per una certa malattia da esito positivo,
ossia segnala la presenza della malattia, nel 90% dei malati. Da invece esito
negativo, ossia non segnala la presenza della malattia, nel 99% dei sani. La
prevalenza della malattia nella popolazione, cioè la frazione di malati, è del 2%.
Si vuole indagare sulla probabilità di ottenere un falso positivo da quell’esame
diagnostico. Un falso positivo è un paziente che ha esito positivo dell’esame ma
non ha la malattia.

A = "esame diagnostico positivo" ; B = "popolazione malata" P(B) = 2/100; C = "popolazione sana" P(C = 98/100\
A|C = "sani con esito positivo" P(A|C) = (1/100)\
`P(C|A) = P(C) * P(A|C) / P(A) = P(C) * P(A|C) / (P(B) * P(A|B) + P(C) * P(A|C)) = 98/100 * 1/100 / (90/100 * 2/100 + 98/100 * 1/100) = 0.352518 = 35.3%`\
N.B.: La potenza del teorema di Bayes stà nel fatto che si può riapplicare per ottenere risultati più certi, in questo caso abbiamo verificato che ho il 35.3% di probabilità di essere sano (e il 100-35.3 = 64.7% di essere malato).\

* Se dovessi ripetere il test (dopo averlo fatto una volta) cosa succederebbe? Le mie probabilità aumenterebbero o no? Vediamo:

A = "esame diagnostico positivo" ; B = "risultato test precedente (malato)" P(B) = 676/1000 ; C = "risultato test precedente (sano)" 353/1000 ; A|C = "falsi positivi" = 1/100\
`P(C|A) = P(C) * P(A|C) / (P(B) * P(A|B) + P(C) * P(A|C)) = (353/1000 * 1/100) / ((353/1000 * 1/100) + (676/1000 * 90/100)) = 0.0057686 = 0.58%`

* Quindi, dopo aver fatto il test 1 volta avevi il 35.3% di essere sano, dopo averlo ripetuto una seconda volta avevi lo 0.58% di probabilità di esserlo. Volendo puoi ripetere ancora il test, e vedrai che la probabilità diminuirà ancora.
## ESEMPIO 2 (con dati reali di un'azienda farmaceutica)
Il 60% delle donne ha aneuploidie cromosomiche fetali, l'azienda GENOMA ha un test con accuratezza 99.1% (il 99.1% dei malati viene riconosciuto come tale), e con lo 0.1% di falsi positivi (lo 0.1% di sani viene erroneamente dichiarato malato).\
Calcola la probabilità che il test sbagli?

A = "donna con aneuploidie cromosomiche fetali" (60/100) ; notA = "donna senza aneuploidie cromosomiche fetali" (40/100) ; B = "test positivo" ; (B|notA) = "falso positivo"(1/1000) ; P(B|A) = "test positivo se hai la aneuploidie cromosomiche fetali" (991/1000)\
`P(notA|B) = P(B|notA) * P(notA) / (P(B|notA) * P(notA) + P(B|A) * P(A)) = (1/1000 * 40/100) / ((1/1000 * 40/100) + (991/1000) * (60/100)) = 0.000672268 = 0.06723%`\
Hai lo 0.067% di probabilità di essere sano, come vedi nei test medici reali, non saltano fuori numeri assurdi come 35%, e sono affidabili e a prova di bayes.
## Altri esempi dal libro
Esempio 5.2 In un certo ufficio il 60% dei documenti è scritto in Word, il 30%
in HTML, il 10% in LATEX. Si sa che superano le 12 pagine il 50% dei documenti
scritti in Word, il 10% dei documenti scritti in HTML, il 20% dei documenti
scritti in LATEX. Si estrae a caso un documento e si constata che supera le 12
pagine. Con quale probabilità risulta scritto in LATEX?\
E = "documento con pagine > 12"\
A = "documento in word" (6/10)\
B = "documento in html" (3/10)\
C = "documento in latex"(1/10)\
E|A = "documento word con pagine > 12"(5/10)\
E|B = "documento html con pagine > 12"(1/10)\
E|C = "documento latex con pagine > 12"(2/10)\
`P(C|E) = P(E|C) * P(C) / P(E) = P(E|C) * P(C) / (P(E|C) * P(C) + P(E|B) * P(B) + P(E|A) * P(A)) = (2/10 * 1/10) / ((2/10 * 1/10) + (1/10 * 3/10) + (5/10 * 6/10)) = 0.05714285714 = 5.72%`

Esempio 5.3 Un’urna contiene 10 palline nere e 90 bianche. Una seconda
urna contiene 50 palline nere e 50 bianche. Una terza urna contiene 90 palline
nere e 10 bianche. Uno sperimentatore sceglie a caso un’urna fra le tre con
equiprobabilita, poi estrae a caso, con reinserimento, due palline dall’urna scelta.
Si determini la probabilità che l’urna scelta sia stata quella con 50 nere, se le
palline estratte risultano, senza tener conto dell’ordine di estrazione, una nera
e una bianca.\
E = "1 bianca e 1 nera"\
A = "urna con 90 nere" (1/3)\
B = "urna con 50 nere" (1/3)\
C = "urna con 10 nere" (1/3)\
B|E = "urna con 50 nere, dopo aver estratto 1 bianca e una nera"\
E|A = "estrarre una bianca e una nera da un urna con 90 nere"\
E|B = "estrarre una bianca e una nera da un urna con 50 nere"\
E|C = "estrarre una bianca e una nera da un urna con 10 nere"\
P(E|A) = Bi(2, 0.9) = (2 su 1) * (9/10)^1 * (1/10)^1 = 0.18\
P(E|B) = Bi(2, 0.5) = (2 su 1) * (5/10)^1 * (5/10)^1 = 0.50\
P(E|C) = Bi(2, 0.1) = (2 su 1) * (1/10)^1 * (9/10)^1 = 0.18\
`P(B|E) = P(E|B) * P(B) / (P(E|B) * P(B) + P(E|A) * P(A) + P(E|C) * P(C)) = (5/10 * 1/3) / ((5/10 * 1/3) + (0.18 * 1/3) + (0.18 * 1/3)) = 0.5813953488 = 58.1%`