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

```
1 0 0 0
2 2 0 0
2 5 3 1
0 1 0 6

1 0 0 0
2 2 0 0
2 5 3 0
0 1 0 6

1 1 0 2
```

$$
\begin{aligned}
& A\rightarrow && \mathrm{pred}~A'\\
& A'\rightarrow && EA''\\
& A''\rightarrow && ,EA'' && | ~\epsilon\\
& B\rightarrow && A && | ~EB'\\
& B'\rightarrow && =E && | ~>E\\
& E\rightarrow && \mathrm{id}E' && | ~\mathrm{num}E'\\
& E'\rightarrow && +E && | ~\epsilon\\
& P\rightarrow && (PP' && | ~\mathrm{not}~P && | ~B\\
& P'\rightarrow && \mathrm{or}~P) && | ~\mathrm{and}~P)\\
\end{aligned}
$$

$$
\begin{aligned}
&& A: & (\mathrm{pred}~A')\mapsto\{\mathrm{pred}\}\\
&& A': & (EA'')\mapsto\{\mathrm{id},\mathrm{num}\}\\
&& A'': &(,EA'')\mapsto\{,\}~|~(\epsilon)\mapsto\{\epsilon\}\\
&& B: &  (A)\mapsto\{\mathrm{pred}\}~|~(EB')\mapsto\{\mathrm{id},\mathrm{num}\}\\
&& B': & (=E)\mapsto\{=\}~|~(>E)\mapsto\{>\}\\
&& E: &  (\mathrm{id}E')\mapsto\{\mathrm{id}\}~|~(\mathrm{num}E')\mapsto\{\mathrm{num}\}\\
&& E': & (+E)\mapsto\{+\}~|~(\epsilon)\mapsto\{\epsilon\}\\
&& P: &  ((PP')\mapsto\{(\}~|~(\mathrm{not}P)\mapsto\{\mathrm{not}\}~|~(B)\mapsto\{\mathrm{id},\mathrm{num},\mathrm{pred}\}\\
&& P': &(\mathrm{or}~P))\mapsto\{\mathrm{or}\}~|~(\mathrm{and}~P))\mapsto\{\mathrm{and}\}& \\
\end{aligned}
$$

$$
\begin{aligned}
FOLLOW(A)=&~ \{\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(A')=&~ \{\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(A'')=&~ \{\$,),\mathrm{or},\mathrm{and}\}\\
FOLLOW(B)=&~ \{\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(B')=&~ \{\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(E)=&~ \{,~,=,>,\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(E')=&~ \{,~,=,>,\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(P)=&~ \{\$,),\mathrm{or},\mathrm{and}\} \\
FOLLOW(P')=&~ \{)\} \\
\end{aligned}
$$