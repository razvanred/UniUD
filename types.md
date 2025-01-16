# Titolo

The definition of the set of types $\Tau_1$ is defined as follows.

$$
\begin{aligned}
&\Tau_1=\{\bool,\char,\int,\float,\string\}\cup\{\&\tau\vert\tau\in\Tau_1\}\cup\{[n]\tau|n\in\mathbb{N},\tau\in\Tau_1\}\\
&\Tau_2=\{\void\}\cup\Tau_2\\
\end{aligned}
$$

We introduce the **compatibility** relation $\leq\subseteq\Tau\times\Tau$, $\tau_1\leq\tau_2$ denotes that the type $\tau_1$ is compatible with the type $\tau_2$. Moreover, it holds that:

* $\forall\tau\in\Tau\quad\tau\leq\tau$.
* $\forall n\in\mathbb{N}\wedge\tau_1,\tau_2\in\Tau \quad \tau_1\leq\tau_2\longrightarrow\neg([n]\tau_1\leq[n]\tau_2) \wedge\neg(\&\tau_1\leq\&\tau_2)$.

<!-- This is a comment -->

Notice that the partially ordered set $(\Tau,\leq)$ has the least upper bound property. We denote it with $\sup$ defined as follow:

$$
\tau_1\sup\tau_2=\begin{cases}
\tau_1&\tau_1=\tau_2\\
\int & (\tau_1 = \int\land\tau_2=\char)\lor(\tau_1=\char\land\tau_2=\int)\\
\float & (\tau_1 = \float\land\tau_2=\char)\lor(\tau_1=\char\land\tau_2=\float)\\
\float & (\tau_1 = \int\land\tau_2=\float)\lor(\tau_1=\float\land\tau_2=\int)\\
\mathsf{error} & \mathrm{otherwise}
\end{cases}
$$

## Type System

The type system is described by the following formalism.

$$
\begin{aligned}
& \Tau_2'=\Tau_2\times\{l_{expr},r_{expr}\}\\
& \mathbb{D}=\mathbb{S}\times \Tau_2'\\
& : \; \subseteq  \mathbb{S} \times \Tau_2'\\
& env = \{ (x,\tau,l) \vert (x,\tau,l) \in :, x \in \mathbb{S}, (\tau,l) \in \Tau_2' \}  \subset \mathbb{D} \times \mathbb{D}\\
\end{aligned}
$$

From now on, we will refer to $env$ as an ordered list of the form $\varnothing,x_1:(\tau,l),\ldots,x_n:(\tau,l)$

## Rules

Letterali

$$
\frac{Type=\begin{cases}
\mathtt{bool}\mapsto\bool\\
\mathtt{char}\mapsto\char\\
\mathtt{integer}\mapsto\int\\
\mathtt{float}\mapsto\float\\
\mathtt{string}\mapsto\string
\end{cases}}{env\vdash_E literal:Type(literal)}
$$

Operatori binari

$$
\frac{\mathtt{binaryMathOp}\in\{\mathtt{+},\mathtt{-},\mathtt{*},\mathtt{/},\mathtt{**}\}\quad env\vdash_Ee_1:\tau_1\quad env\vdash_Ee_2:\tau_2\quad(\tau_1\sup\tau_2)\leq\float}{env\vdash_E e_1\;\mathtt{binaryOp}\;e_2\;:\tau_1\sup\tau_2}
$$

$$
\frac{env\vdash_Ee_1:\tau_1\quad env\vdash_Ee_2:\tau_2\quad(\tau_1\sup\tau_2)\leq\int}{env\vdash_E e_1\;\mathtt{\%}\;e_2\;:\tau_1\sup\tau_2}
$$

$$
\frac{env\vdash_Ee:\bool}{env\vdash_E\;\mathtt{!}e\;:\bool}
$$

$$
\frac{\mathtt{binaryOp}\in\{\mathtt{||},\mathtt{\&\&}\}\quad env\vdash_Ee_1:\bool\quad env\vdash_Ee_2:\bool}{env\vdash_E e_1\;\mathtt{binaryOp}\;e_2\;:\bool}
$$

Comparatori

$$
\frac{\mathtt{comparatorOp}\in\{\mathtt{==},\mathtt{!=},\mathtt{<},\mathtt{<=},\mathtt{>},\mathtt{>=}\}\quad env\vdash_Ee_1:\tau_1\quad env\vdash_Ee_2:\tau_2\quad(\tau_1\sup\tau_2)\leq\float}{env\vdash_E e_1\;\mathtt{comparatorOp}\;e_2\;:\bool}
$$

negazione

$$
\frac{env\vdash_Ee:\tau\quad\tau\leq\float}{env\vdash_E\mathtt{-}e\;:(\tau\sup\int)}
$$

dichiarazione

## Altro

$$
\frac{env \vdash_E l:\tau_1 \qquad env \vdash_E r:\tau_2\qquad\tau_1\geq\tau_2 }{env\vdash_S l~bop~r }
$$

$$
bop\in\{\mathtt{:=},\mathtt{*=},\mathtt{+=},\mathtt{/=},\mathtt{-=},\mathtt{\^=},\mathtt{\&=},\mathtt{|=}\}
$$

$$
\shdw\quad\clash\quad\sup
$$

$$
\frac{prelude}{}
$$

$$
\frac{env\vdash a:\tau_1,Lv\quad env\vdash a:\tau_2,Rv\quad \sup()}{}
$$

$$
\frac{env\vdash_{RE} e_1:\tau_1\quad env\vdash e_2:\tau_2\quad \sup(\tau_1,\tau_2)<\int}{env\vdash e_1*e_2:\int}
$$

