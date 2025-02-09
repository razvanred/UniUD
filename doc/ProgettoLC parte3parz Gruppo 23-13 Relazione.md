Versioni degli strumenti usati:

BNFC - 2.9.5

GHC - 9.8.2

Happy - 2.0.2

Alex - 3.5.1.0

Il progetto è stato strutturato in moduli distinti, che corrispondono alle diverse fasi della compilazione. Un primo modulo è composto da Lexer e Parser (per costruirli si è fatto uso di BNFC). Le direttive di BNFC specificano un Abstract Syntax Tree polimorfo. Il risultato della fase di lexing e parsing è l'istanziazione di quest'ultimo con le posizioni (riga e colonna) dei token. Questo AST viene trasformato in un AST semplificato (sempre polimorfo), la cui sintassi è stata da noi specificata (modulo ```AST.hs``` ), che viene utilizzato come base per la fase di analisi di semantica statica e per quella successiva di generazione del Three Address Code, ciascuna corrispondente a un modulo separato.

Un programma nel linguaggio implementato non prevede un entrypoint esplicito (es. una funzione main), ma è costituito da un blocco, che si compone da una serie di istruzioni, a loro volta statement o espressioni.  

In qualsiasi punto all'interno dei blocchi è possibile dichiarare costanti, variabili e funzioni. Tutte le dichiarazioni prevedono la contemporanea inizializzazione. Costanti e variabili sono visibili solo dal punto di dichiarazione in poi, mentre per quanto riguarda le dichiarazioni di funzioni abbiamo la full visibility.

Le costanti sono gestite come valori determinati al momento della compilazione, possono essere inizializzate con espressioni che comprendono altre costanti anche non precedentemente dichiarate.

Le variabili possono essere inizializzate con espressioni generiche. Variabili e costanti condividono lo stesso namespace, mentre le funzioni/procedure hanno un namespace separato. Le costanti devono essere scritte con caratteri maiuscoli (il caso contrario viene generato un warning). 

Nella seguente tabella elenchiamo gli operatori usati con la semantica consueta. L'operatore & è usato per l'operazione di referenziazione, il $ per la dereferenziazione.

| Operatore | Precedenza  | Associatività   |
| :----:    |    :----:   |          :----: |
| \|\|      | 1           | Sinistra        |
| &&        | 2           | Sinistra        |
|!          | 3           |                 |
|==, !=, <, <=, >, >=| 4  |                 |
|+, - (binario)| 5        | Sinistra        |
|*, /, %    | 6           | Sinistra        |
|**         | 7           | Destra          |
|- (unario) | 8           |                 |
|++, -- (pre e post)| 9   |                 |
|&, $       | 10          |                 |

Per il passaggio di parametri le modalità ammesse sono per valore e per riferimento.

I casting impliciti sono ammessi solamente verso l'alto (secondo il reticolo dei tipi definito nella seguente sezione) e ogni tipo numerico ha una sintassi letterale associata.

A livello di linguaggio ammettiamo il tipo stringa con l'indicizzazione come unica operazione consentita. 

Per la generazione di Three Address Code abbiamo deciso di utilizzare uno stato costruito sulla base di una monade di stato e un "monad transformer" ```StateT s (m :: * -> *)``` (entrambi forniti dalla libreria ```Control.Monad.Trans.State```).

Lo stato comprende i seguenti elementi:
- un intero usato per generare gli identificatori dei temporanei e delle label,
- un intero per stringhe statiche
- una lista di label
- una lista di istruzioni TAC
- una lista di label sospese per un uso successivo (che permette l'uso della tecnica fall-through nella generazione di TAC per le espressioni booleane)
- una lista di coppie di label per permettere la gestione delle istruzioni di break e continue all'interno del costrutto di iterazione indeterminata (```while ... do ...```)
- una mappa con valori stringhe e label identificative

Per gestire la definizione di funzioni innestate lo stato viene salvato e ripristinato grazie all'uso del "monad transformer". 

Negli assegnamenti la valutazione dell'l-value precede quella dell'r-value. La valutazione delle espressioni avviene da sinistra a destra, così come quella degli argomenti delle funzioni o procedure. La valutazione delle espressioni booleane avviene ovunque con modalità short-cut (guardie, passaggio dei parametri, assegnamenti).

# Type System

The character ; is used a separator between statements.

The definition of the set of types $\mathbb T_1$ is defined as follows.

$$
\begin{aligned}
&\mathbb T_1=\{\bool,\char,\int,\float,\string\}\cup\{\&\tau\vert\tau\in\mathbb T_1\}\cup\{[n]\tau|n\in\mathbb N,\tau\in\mathbb{T_1}\}\\
&\mathbb T_2=\mathbb{T_1}\cup\{\void\}\cup\{\mu_1\tau_1 \times \ldots \times \mu_n\;\tau_n \mapsto \tau\; |\tau \in \mathbb{T_1}\cup{void},\forall i (\mu_i\in\{byRef,byVal\}\;\wedge \; \tau_i\in \mathbb{T_1}) \}\\
\end{aligned}
$$

We introduce the **compatibility** relation $\leq\:\subseteq\mathbb T_2\times\mathbb T_2$, where $\tau_1\leq\tau_2$ denotes that the type $\tau_1$ is compatible with the type $\tau_2$. Moreover, it holds that:

* $\forall\tau\in\mathbb T_2\quad\tau\leq\tau$.
* $\forall n\in\mathbb N\wedge\tau_1,\tau_2\in\mathbb T_2\quad\tau_1\leq\tau_2\longrightarrow\neg([n]\tau_1\leq[n]\tau_2) \wedge\neg(\&\tau_1\leq\&\tau_2)$.

<!-- This is a comment -->

Notice that the partially ordered set $(\mathbb T_2,\leq)$ has the least upper bound property. We denote it with $\sup$ defined as follows:

$$
\tau_1\sup\tau_2=\begin{cases}
\tau_1 & \tau_1=\tau_2\\
\int & (\tau_1=\int\land\tau_2=\char)\lor(\tau_1=\char\land\tau_2=\int)\\
\float & (\tau_1=\float\land\tau_2=\char)\lor(\tau_1=\char\land\tau_2=\float)\\
\float & (\tau_1=\int\land\tau_2=\float)\lor(\tau_1=\float\land\tau_2=\int)\\
\mathsf{error} & \mathrm{otherwise}
\end{cases}
$$

## Variable declarations ($V$)

variable declaration

$$
\frac{env\vdash_Ee:(\tau_2,\rvalue)\quad\tau_1\geq\tau_2}{\extenv,env\vdash_V|\mathtt{var}\;id\mathtt{:}\tau_1\;\mathtt{:=}\;e|:\{(id,\tau_1)\}}
$$

sequences

$$
\frac{env_2\vdash_Sstmt\quad \extenv,env_1\vdash_V\vdecl:env_3\quad env_2=\extenv\shdw env_1}{\extenv,env_1\vdash_V|stmt\;\mathtt{;}\;\vdecl|:env_3}
$$

$$
\frac{\extenv,env_1\vdash_V\vdecl:env_2\quad env_3\vdash_Sstmt\quad env_3=\extenv\shdw(env_1\clash env_2)}{\extenv,env_1\vdash_V|\vdecl\;\mathtt{;}\;stmt|:env_2}
$$

$$
\frac{\extenv,env_1\vdash_V\vdecl_1:env_2\quad \extenv,env_3\vdash_V\vdecl_2:env_4\quad env_3=env_1\clash env_2}{\extenv,env_1\vdash_V|\vdecl_1\;\mathtt{;}\;\vdecl_2|:env_2\clash env_4}
$$

$$
\frac{env_2\vdash_Sstmt\quad env_2=\extenv\shdw env_1}{\extenv,env_1\vdash_Vstmt:\varnothing}
$$

## Function declarations ($F$)

We define $\fonly{env}=\{(id,...\mapsto\tau):(id,...\mapsto\tau)\in env\}$, such that $\fonly{env}$ contains only function definitions. This lets us avoid full visibility for variable declarations.

function declaration

$$
\begin{aligned}
&\tau=m^p_1\tau^p_1\times...\mapsto\tau^f\\
&\frac{env_2,env_3\vdash_Ff_{body}:env_{x}\quad env_2=\extenv\shdw env_1\quad env_3=\left\{(id^f,\tau),(\mathtt{return},\tau^f),(id_1^p,\tau^p_1),...\right\}}{\extenv,env_1\vdash_F|\mathtt{def}\;id^f\;\mathtt{(}m^p_1id^p_1\mathtt{:}\tau^p_1,\;...\mathtt{)}:\tau^f\;\{f^{body}\}|:\{(id^f,\tau)\}}
\end{aligned}
$$

sequences

$$
\frac{\extenv,env_5\vdash_V\vdecl:env_2\quad \extenv,env_3\vdash_F\fdecl:env_4\quad env_3=env_1\clash env_2\quad env_5=env_1\clash\fonly{env_4}}{\extenv,env_1\vdash_F|\vdecl\;\mathtt{;}\;\fdecl|:env_2\clash env_4}
$$

$$
\frac{\extenv,env_1\vdash_F\fdecl:env_2\quad \extenv,env_3\vdash_V\vdecl:env_4\quad env_3=env_1\clash env_2}{\extenv,env_1\vdash_F|\fdecl\;\mathtt{;}\;\vdecl|:env_2\clash env_4}
$$

$$
\frac{\extenv,env_4\vdash_F\fdecl_1:env_2\quad \extenv,env_3\vdash_F\fdecl_2:env_4\quad env_3=env_1\clash env_2\quad env_5=env_1\clash\fonly{env_4}}{\extenv,env_1\vdash_F|\fdecl_1\;\mathtt{;}\;\fdecl_2|:env_2\clash env_4}
$$

$$
\frac{\extenv,env_1\vdash_V\vdecl:env_2}{\extenv,env_1\vdash_F\vdecl:env_2}
$$

## Expressions

$$
\frac{env(id)=\tau\quad\tau\in\mathbb{T_2}}{env\vdash_E id:(\tau,\lvalue)}
$$

indexed accessor

$$
\frac{env\vdash_Ee_1:(\mathsf{[n]}\tau_1,\omega)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad\tau_2\leq\int,n\in\mathbb N, \omega\in\{\lvalue, \rvalue \} }{env\vdash_E|e_1\mathtt{[}e_2\mathtt{]}|:(\tau_1,\omega)}
$$

$$
\frac{env\vdash_Ee_1:(\string,\lvalue)\quad env\vdash_Ee_2:(\tau,\rvalue)\quad\tau\leq\int}{env\vdash_E|e_1\mathtt{[}e_2\mathtt{]}|:(\char,\rvalue)}
$$

literals

$$
\frac{Type=\begin{cases}
\mathtt{bool}\mapsto\bool\\
\mathtt{char}\mapsto\char\\
\mathtt{integer}\mapsto\int\\
\mathtt{float}\mapsto\float\\
\mathtt{string}\mapsto\string
\end{cases}}{env\vdash_E\; literal:(Type(literal),\rvalue)}
$$

array literal

$$
\frac{\exist\tau\forall i(env\vdash_E e_i:(\tau_i,\rvalue)\;\wedge\; \tau_i\leq \tau )   \not\exists i_1,i_2(env\vdash_E e_{i_1}:(\tau_1,\rvalue)\;\wedge\; env\vdash_E e_{i_2}:(\tau_2,\rvalue)\; \wedge \;\tau_1 \not= \tau_2)}{env\vdash_E \mathtt{[}e_1 \ldots e_n\mathtt{]}:(\mathsf{[n]}\tau,\rvalue)}
$$

binary operators

$$
\frac{\mathtt{binaryMathOp}\in\{\mathtt{+},\mathtt{-},\mathtt{*},\mathtt{/},\mathtt{**}\}\quad env\vdash_Ee_1:(\tau_1,\rvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad(\tau_1\sup\tau_2)\leq\float}{env\vdash_E|e_1\;\mathtt{binaryOp}\;e_2|:(\tau_1\sup\tau_2,\rvalue)}
$$

$$
\frac{env\vdash_Ee_1:(\tau_1,\rvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad(\tau_1\sup\tau_2)\leq\int}{env\vdash_E|e_1\;\mathtt{\%}\;e_2|:(\tau_1\sup\tau_2,\rvalue)}
$$


$$
\frac{\mathtt{binaryOp}\in\{\mathtt{||},\mathtt{\&\&}\}\quad env\vdash_Ee_1:(\bool,\rvalue)\quad env\vdash_Ee_2:(\bool,\rvalue)}{env\vdash_E|e_1\;\mathtt{binaryOp}\;e_2|:(\bool,\rvalue)}
$$

unary operators

$$
\frac{\mathtt{unariMathOP}\in\{\mathtt{++},\mathtt{--}\}\quad env\vdash_Ee:(\tau,\lvalue)\quad \tau\leq\float }{env\vdash_E\;\mathtt{unariMathOP}\;e\;:(\tau,\rvalue)}
$$

$$
\frac{\mathtt{unariMathOP}\in\{\mathtt{++},\mathtt{--}\}\quad env\vdash_Ee:(\tau,\lvalue)\quad \tau\leq\float }{env\vdash_E\;e\;\mathtt{unariMathOP}\;:(\tau,\rvalue)}
$$

$$
\frac{env\vdash_Ee:(\bool,\rvalue)}{env\vdash_E\;\mathtt{!}e\;:(\bool,\rvalue)}
$$

$$
\frac{env\vdash_Ee:(\tau,\rvalue)\quad\tau\leq\float}{env\vdash_E\mathtt{-}e\;:(\tau\sup\int,\rvalue)}
$$

$$
\frac{env\vdash_Ee:(\tau,\lvalue)}{env\vdash_E\;\&e\;:(\&\tau,\rvalue)}
$$

$$
\frac{env\vdash_Ee:(\&\tau,\lvalue)}{env\vdash_E\;\$e\;:(\tau,\lvalue)}
$$

comparators

$$
\frac{\mathtt{comparatorOp}\in\{\mathtt{==},\mathtt{!=},\mathtt{<},\mathtt{<=},\mathtt{>},\mathtt{>=}\}\quad env\vdash_Ee_1:(\tau_1,\rvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad(\tau_1\sup\tau_2)\leq\float}{env\vdash_E|e_1\;\mathtt{comparatorOp}\;e_2|:(\bool,\rvalue)}
$$

function call

$$
\frac{env(id)=\mu_1\;\tau_1 \times \ldots \times \mu_n\;\tau_n \mapsto \tau \quad \forall i( (\mu_i = byRef \implies env\vdash_E e_i:(\tau_i',\lvalue) \wedge \tau_i' \sup \tau_i = \tau_i ) \wedge (\mu_i = byVal \implies env\vdash_E e_i:(\tau_i,\rvalue)) \wedge \tau_i' \sup \tau_i = \tau_i   ) }{env\vdash_E\;id(e_1 \ldots e_n):(\tau,\rvalue)}
$$

value

$$
\frac{env\vdash_Ee:(\tau,\lvalue)}{env\vdash_E e:(\tau,\rvalue)}
$$

## Statements ($S$)

block

$$
\frac{env_1,\varnothing\vdash_F\fdecl:env_x}{env\vdash_S\mathtt{\{}\fdecl\mathtt{\}}}
$$

assignment

$$
\frac{env\vdash_Ee_1:(\tau_1,\lvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad\tau_1\geq\tau_2}{env\vdash_Se_1\;\mathtt{:=}\;e_2}
$$

$$
\frac{\mathtt{o}\in\{+,-,*,/,**,\&\&,|| \} \quad env\vdash_E e_1:(\tau_1,\lvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad\tau_1\geq\tau_2 }{env\vdash_Se_1\;\mathtt{o=}\;e_2}
$$

branch

$$
\frac{env\vdash_E e:(bool,\rvalue)\quad env\vdash_S b}{env\vdash_S if\; e\;b}
$$

$$
\frac{env\vdash_E e:(bool,\rvalue)\quad env\vdash_S b_1 \quad env\vdash_S b_2}{env\vdash_S if\; e\;b_1\; b_2}
$$

$$
\frac{env\vdash_E e:(bool,\rvalue)\quad env\vdash_S b}{env\vdash_S while\; e\;b}
$$

jump

$$
\frac{env(break) = void}{env\vdash_S break}
$$

$$
\frac{env(continue) = void}{env\vdash_S continue}
$$

$$
\frac{env(return)= \tau \quad env\vdash_E e : (\tau',\rvalue)\quad \tau' \leq \tau }{env\vdash_S return\; e }
$$


$$
\frac{env(return)= void}{env\vdash_S return\;}
$$
