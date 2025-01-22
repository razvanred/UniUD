# Titolo

## cose da sistemare

1. aggiungere tipi funzioni a $\mathbb T_2$
2. come separatore usianmo ; invece di `\n`

---

The definition of the set of types $\mathbb T_1$ is defined as follows.

$$
\begin{aligned}
&\mathbb T_1=\{\bool,\char,\int,\float,\string\}\cup\{\&\tau\vert\tau\in\mathbb T_1\}\cup\{[n]\tau|n\in\mathbb N,\tau\in\mathbb{T_1}\}\\
&\mathbb T_2=\mathbb{T_1}\cup\{\void\}\\
\end{aligned}
$$

#### immagine del lattice

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

## Type System

$\mathbb S$ contains every sintactically valid language subterm.

### Variable declarations ($V$)

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

### Function declarations ($F$)

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

### Expressions



$$
\frac{env(id)=\tau\quad\tau\in\mathbb{T_2}}{env\vdash_E id:\tau,\lvalue}
$$

indexed accessor

$$
\frac{env\vdash_Ee_1:(\mathsf{[e_2]}\tau_1,\omega)\quad env\vdash_Ee_2:\tau_2\quad\tau_2\leq\int}{env\vdash_E|e_1\mathtt{[}e_2\mathtt{]}|:(\tau_1,\omega)}
$$

$$
\frac{env\vdash_Ee_1:(\string,\omega)\quad env\vdash_Ee_2:\tau\quad\tau\leq\int}{env\vdash_E|e_1\mathtt{[}e_2\mathtt{]}|:(\char,\rvalue)}
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

binary operators

$$
\frac{\mathtt{binaryMathOp}\in\{\mathtt{+},\mathtt{-},\mathtt{*},\mathtt{/},\mathtt{**}\}\quad env\vdash_Ee_1:(\tau_1,\rvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad(\tau_1\sup\tau_2)\leq\float}{env\vdash_E|e_1\;\mathtt{binaryOp}\;e_2|:\tau_1\sup(\tau_2,\rvalue)}
$$

$$
\frac{env\vdash_Ee_1:\tau_1\quad env\vdash_Ee_2:\tau_2\quad(\tau_1\sup\tau_2)\leq\int}{env\vdash_E|e_1\;\mathtt{\%}\;e_2|:\tau_1\sup\tau_2}
$$

$$
\frac{env\vdash_Ee:\bool}{env\vdash_E\;\mathtt{!}e\;:\bool}
$$

$$
\frac{\mathtt{binaryOp}\in\{\mathtt{||},\mathtt{\&\&}\}\quad env\vdash_Ee_1:\bool\quad env\vdash_Ee_2:\bool}{env\vdash_E|e_1\;\mathtt{binaryOp}\;e_2|:\bool}
$$

comparators

$$
\frac{\mathtt{comparatorOp}\in\{\mathtt{==},\mathtt{!=},\mathtt{<},\mathtt{<=},\mathtt{>},\mathtt{>=}\}\quad env\vdash_Ee_1:\tau_1\quad env\vdash_Ee_2:\tau_2\quad(\tau_1\sup\tau_2)\leq\float}{env\vdash_E|e_1\;\mathtt{comparatorOp}\;e_2|:\bool}
$$

negation

$$
\frac{env\vdash_Ee:\tau\quad\tau\leq\float}{env\vdash_E\mathtt{-}e\;:(\tau\sup\int)}
$$

value

$$
\frac{env\vdash_Ee:(\tau,\lvalue)}{env\vdash_E e:(\tau,\rvalue)}
$$

### Statements ($S$)

block

$$
\frac{env_1,\varnothing\vdash_F\fdecl:env_x}{env\vdash_S\mathtt{\{}\fdecl\mathtt{\}}}
$$

assignment

$$
\frac{env\vdash_Ee_1:(\tau_1,\lvalue)\quad env\vdash_Ee_2:(\tau_2,\rvalue)\quad\tau_1\geq\tau_2}{env\vdash_Ee_1\;\mathtt{:=}\;e_2}
$$

# Stuff

$$
\shdw\quad\clash\quad\sup
$$

$$
\frac{prelude}{}
$$

