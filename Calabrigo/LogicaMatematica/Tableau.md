# Tableau
Il tableau è un albero i cui nodi sono formule, ed è costituito per gradi t1,...,tn, dove ogni grado aggiunge uno o due nodi. Il tableau segue le regole dell'FNC e FND per scomporre una formula in letterali. E(n) dove n è un nodo indica l'insieme che etichetta n. Alla fine di un tableau, ovvero quando si è scomposto in letterali, bisogna dire se le varie formule scomposte siano impossibili o compatibili, e in questo ultimo caso, anche dire per che valori siano compatibili.\
Lemma: Se un albero binario è infinito, allora ha un ramo infinito.\
Teorema: L'algoritmo di costruzione dei tableau ha terminazione forte, ovvero termina sempre.\
Un tableau è chiuso se ha letterali complementari in tutte le sue foglie, altrimenti è aperto
