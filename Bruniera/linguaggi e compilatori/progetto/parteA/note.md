Devo calcolare

$$
v(A+B^T)v^T+v(A^T+B)v^T
$$

Dopo che ho calcolato $A+B^T$ basta fare la trasposta del risultato per ottenere $A^T+B$.

$$
\begin{aligned}
&A+B^T=\\
&(A^T+(B^T)^T)^T=&[(X+Y)^T=X^T+Y^T]\\\
&(A^T+B)^T\\
&\iff\\
&A^T+B=\\
&((A^T)^T+B^T)^T=&[(X+Y)^T=X^T+Y^T]\\
&(A+B^T)^T
&\end{aligned}
$$

Per la proprietà distributiva dei dot product dei vettori possiamo raccogliere $v$

$$
v((A+B^T)v^T+(A^T+B)v^T)
$$

per la proprietà distributiva possiamo raccogliere anche $v^T$

$$
v(((A+B^T)+(A^T+B))v^T)
$$

Usiamo di nuovo il trucco delle somme di trasposte insieme alla proprietà commutativa della somma di matrici.

$$
v(((A+B)+(A^T+B^T))v^T)\\
v(((A+B)+(A+B)^T)v^T)
$$

1 0 0 0
2 2 0 0
2 5 3 1
0 1 0 6

1 0 0 0
2 2 0 0
2 5 3 0
0 1 0 6

1 1 0 2

