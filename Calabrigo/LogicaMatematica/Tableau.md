# Tableau
Il tableau è un albero i cui nodi sono formule, ed è costituito per gradi t1,...,tn, dove ogni grado aggiunge uno o due nodi. Il tableau segue le regole dell'FNC e FND per scomporre una formula in letterali. E(n) dove n è un nodo indica l'insieme che etichetta n. Alla fine di un tableau, ovvero quando si è scomposto in letterali, bisogna dire se le varie formule scomposte siano impossibili o compatibili, e in questo ultimo caso, anche dire per che valori siano compatibili.\
Lemma: Se un albero binario è infinito, allora ha un ramo infinito.\
Teorema: L'algoritmo di costruzione dei tableau ha terminazione forte, ovvero termina sempre.\
Un tableau è chiuso se ha letterali complementari in tutte le sue foglie, altrimenti è aperto.\
Se un tableau è chiuso, F è insoddisfacibile, e viceversa.\

# Insiemi di Hintikka
Un insieme di hintikka è un insieme i cui elementi sono le funzioni di un tableau (o di un suo ramo), con la condizione che questi sia aperto e che rispetti le condizioni dell'FNC/FND. Queste condizioni sono:\
* se H è chiuso (contiene letterali complementari), H non è un insieme di hintikka
* se G appartiene ad H, allora H è una doppia negazione con ridotto G
* se G e K appartengono ad H, H è una alpha-formula con ridotti G e K
* se G e K appartengono ad H, H è una beta-formula con ridotti G e H\

Esempio di insieme di Hintikka. H = {p or not(q) -> r; not(p or q); not(q); not(not(q)); q}\

Lemma: Ogni insieme di Hintikka è soddisfacibile. (perchè? perchè non hai letterali complementari).\
Lemma: Se r è un ramo aperto in un tableau, allora l'unione di tutte le funzioni dei nodi figli di r, compreso r, sono un insieme di hintikka.\

In breve, cos'è un insieme di Hintikka? E' un insieme, un'unione di funzioni, che sono tutte soddisfacibili. Viene usato in combo con i tableau, dai quali si possono creare insiemi di Hintikka a patto che i tableau siano aperti; gli insiemi di Hintikka si possono creare anche da rami di tableau, ma devono essere aperti.

