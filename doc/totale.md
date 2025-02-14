
controllare:
- funzioni che devono stare nell'environment iniziale
- assegnamento a slice con letterale
- operazione di accesso a stringhe


Modalita passaggio parametri avveiene di default by value se non viene specificata altrimenti. [DIPENDE SE CONTROLLATO]

Le variabili vengono identificate con il nome seguito da @ e dalla linea di dichiarazione/definizione.

Le dichiarazioni di funzione innestate vengono stampate nel Three Address Code a partire dalla funzione più innestata. Eventuali rimandi a identificatori definiti in scope non locali sono da leggere scorrendo i body delle funzioni verso il basso.

Le label e i temporanei vengono identificati da un intero. Questo intero viene settato a 0 a ogni dichiarazione di funzione e viene condiviso, all'interno di ogni funzione, per label e temporanei.

Dangling pointers ???

```

```

