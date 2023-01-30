# Il teorema di Bayes

Il teorema di bayes è un modo di capire quanto sia probabile un evento condizionale $A\mid B$, a patto di avere informazioni sulle probabilità di $B\mid A$, $A$ e $B$:

$$
P(A\mid B)=\frac{P(A)\cdot P(B\mid A)}{P(B)}
$$

Dove possiamo scrivere $P(B)$, nel caso non lo conoscessimo, come sommatoria per ogni $i\in I$ di $P(A_i)\cdot P(B\mid A_i)$. Quindi posso riscrivere la formula come:

$$
P(A\mid B)=\frac{P(A)\cdot P(B\mid A)}{\sum\limits_{i\in I} P(A_i)\cdot P(B\mid A_i)}
$$

Ora che abbiamo esposto le formule, possiamo provare a capirle. Sarà utile vedere gli esempi per avere più chiaro il concetto, ma praticamente se noi vogliamo sapere quanto spesso accade $A\mid B$, noi dividiamo le volte che accadono sia $A$ che $B$ per la somma delle volte che accadono $B$ e $A$, $B$ e $C$, $\ldots$, $B$ e $Z$. Quindi cerco: $A\mid B=\frac{A\cap B}{B\cap A+B\cap C+\ldots+B\cap Z}$.

Facciamo un esempio per chiarire il concetto, e l'importanza di bayes. Pensandola in questo modo torno alla formula per trovare $P(A\mid B)=\frac{P(B\mid A)\cdot P(A)}{P(B)}=\frac{P(B\cap A)}{P(B)}$; per capire bayes conviene ragionare su questa formula, ma ora andiamo agli esempi.

## Esempio 1 (con dati del libro)

Un esame diagnostico per una certa malattia da esito positivo, ossia segnala la presenza della malattia, nel 90% dei malati. Da invece esito negativo, ossia non segnala la presenza della malattia, nel 99% dei sani. La prevalenza della malattia nella popolazione, cioè la frazione di malati, è del 2%. Si vuole indagare sulla probabilità di ottenere un falso positivo da quell'esame diagnostico. Un falso positivo è un paziente che ha esito positivo dell'esame ma non ha la malattia.

$A$ = "esame diagnostico positivo", $B$ = "popolazione malata" $P(B)=\frac{2}{100}$, $C$ = "popolazione sana" $P(C)=\frac{98}{100}$, $A\mid C$ = "sani con esito positivo" $P(A\mid C)=\frac{1}{100}$.

$$
P(C\mid A)=P(C)\cdot\frac{P(A\mid C)}{P(A)}=P(C)\cdot\frac{P(A\mid C)}{P(B)\cdot P(A\mid B)+P(C)\cdot P(A\mid C)}\\
=\frac{98}{100}\cdot\frac{\frac{1}{100}}{\frac{90}{100}\cdot\frac{2}{100}+\frac{98}{100}\cdot\frac{1}{100}}=0.352518=35.3\%.
$$

> La potenza del teorema di Bayes stà nel fatto che si può riapplicare per ottenere risultati più certi, in questo caso abbiamo verificato che ho il 35.3% di probabilità di essere sano (e il 100 - 35.3 = 64.7% di essere malato).

Se dovessi ripetere il test (dopo averlo fatto una volta) cosa succederebbe? Le mie probabilità aumenterebbero o no? Vediamo:

$A$ = "esame diagnostico positivo", $B$ = "risultato test precedente (malato)" $P(B)$ = $\frac{676}{1000}$, $C$ = "risultato test precedente (sano)" $\frac{353}{1000}$, $A\mid C$ = "falsi positivi" = $\frac{1}{100}$.

$$
P(C\mid A)=\frac{P(C)\cdot P(A\mid C)}{P(B)\cdot P(A\mid B)+P(C)\cdot P(A\mid C)}=\frac{\frac{353}{1000}\cdot \frac{1}{100}}{\frac{353}{1000}\cdot {1}{100}+\frac{676}{1000}\cdot\frac{90}{100}}=0.0057686=0.58\%
$$

Quindi, dopo aver fatto il test 1 volta avevi il 35.3% di essere sano, dopo averlo ripetuto una seconda volta avevi lo 0.58% di probabilità di esserlo. Volendo puoi ripetere ancora il test, e vedrai che la probabilità diminuirà ancora.

## Esempio 2 (con dati reali di un'azienda farmaceutica)

Il 60% delle donne ha aneuploidie cromosomiche fetali, l'azienda GENOMA ha un test con accuratezza 99.1% (il 99.1% dei malati viene riconosciuto come tale), e con lo 0.1% di falsi positivi (lo 0.1% di sani viene erroneamente dichiarato malato).\
Calcola la probabilità che il test sbagli?

$A$ = "donna con aneuploidie cromosomiche fetali" $\frac{60}{100}$, $\neg A$ = "donna senza aneuploidie cromosomiche fetali" $\frac{40}{100}$, $B$ = "test positivo", $B\mid\neg A$ = "falso positivo" $\frac{1}{1000}$, $P(B\mid A)$ = "test positivo se hai la aneuploidie cromosomiche fetali" $\frac{991}{1000}$.

$$
P(\neg A\mid B)=\frac{P(\neg A)\cdot P(B\mid\neg A)}{P(B\mid\neg A)\cdot P(\neg A)+P(B\mid A)\cdot P(A)}\\
=\frac{\frac{1}{1000}\cdot\frac{40}{100}}{\frac{1}{1000}\cdot\frac{40}{100}+\frac{991}{1000}\cdot\frac{60}{100}}=0.000672268=0.06723\%
$$

Hai lo 0.067% di probabilità di essere sano, come vedi nei test medici reali, non saltano fuori numeri assurdi come 35%, e sono affidabili e a prova di bayes.

## Altri esempi dal libro

In un certo ufficio, il 60% dei documenti è scritto in Word, il 30% in HTML, il 10% in LATEX. Si sa che superano le 12 pagine il 50% dei documenti scritti in Word, il 10% dei documenti scritti in HTML, il 20% dei documenti scritti in LATEX. Si estrae a caso un documento e si constata che supera le 12 pagine.\
Con quale probabilità risulta scritto in LATEX?

$E$ = "documento con pagine > 12"\
$A$ = "documento in word" $\frac{6}{10}$\
$B$ = "documento in html" $\frac{3}{10}$\
$C$ = "documento in latex" $\frac{1}{10}$\
$E\mid A$ = "documento word con pagine > 12" $\frac{5}{10}$\
$E\mid B$ = "documento html con pagine > 12" $\frac{1}{10}$\
$E\mid C$ = "documento latex con pagine > 12" $\frac{2}{10}$

$$
P(C\mid E)=\frac{P(C)\cdot P(E\mid C)}{P(E)}=\frac{P(C)\cdot P(E\mid C)}{P(C)\cdot P(E\mid C)+P(B)\cdot P(E\mid B)+P(A)\cdot P(E\mid A)}\\
=\frac{\frac{1}{10}\cdot \frac{2}{10}}{\frac{1}{10}\cdot\frac{2}{10}+\frac{3}{10}\cdot\frac{1}{10}+\frac{6}{10}\cdot\frac{5}{10}}=0.05714285714=5.72\%
$$

Un'urna contiene 10 palline nere e 90 bianche. Una seconda urna contiene 50 palline nere e 50 bianche. Una terza urna contiene 90 palline nere e 10 bianche. Uno sperimentatore sceglie a caso un'urna fra le tre con equiprobabilità, poi estrae a caso, con reinserimento, due palline dall'urna scelta.\
Si determini la probabilità che l'urna scelta sia stata quella con 50 nere, se le palline estratte risultano, senza tener conto dell'ordine di estrazione, una nera e una bianca.

$E$ = "1 bianca e 1 nera"\
$A$ = "urna con 90 nere" $\frac{1}{3}$\
$B$ = "urna con 50 nere" $\frac{1}{3}$\
$C$ = "urna con 10 nere" $\frac{1}{3}$\
$B\mid E$ = "urna con 50 nere, dopo aver estratto 1 bianca e una nera"\
$E\mid A$ = "estrarre una bianca e una nera da un urna con 90 nere"\
$E\mid B$ = "estrarre una bianca e una nera da un urna con 50 nere"\
$E\mid C$ = "estrarre una bianca e una nera da un urna con 10 nere"

$$
P(E\mid A)=Bi(2, 0.9)=\binom 2 1\cdot\left(\frac{9}{10}\right)^1\cdot\left(\frac{1}{10}\right)^1=0.18\\
P(E\mid B)=Bi(2, 0.5)=\binom 2 1\cdot\left(\frac{5}{10}\right)^1\cdot\left(\frac{5}{10}\right)^1=0.50\\
P(E\mid C)=Bi(2, 0.1)=\binom 2 1\cdot\left(\frac{1}{10}\right)^1\cdot\left(\frac{9}{10}\right)^1=0.18
$$

$$
P(B\mid E)=\frac{P(B)\cdot P(E\mid B)}{P(B)\cdot P(E\mid B)+P(A)\cdot P(E\mid A)+P(C)\cdot P(E\mid C)}\\
=\frac{\frac{1}{3}\cdot\frac{5}{10}}{\frac{1}{3}\cdot\frac{5}{10}+\frac{1}{3}\cdot 0.18+\frac{1}{3}\cdot 0.18}=0.5813953488=58.1\%
$$
