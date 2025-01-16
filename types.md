$$
\sup(\tau_1,\tau_2)=\begin{cases}
\tau_1&\tau_1=\tau_2\\
\mathsf{int} & (\tau_1 = \mathsf{int}\land\tau_2=\mathsf{char})\lor(\tau_1=\mathsf{char}\land\tau_2=\mathsf{int})\\
\mathsf{float} & (\tau_1 = \mathsf{float}\land\tau_2=\mathsf{char})\lor(\tau_1=\mathsf{char}\land\tau_2=\mathsf{float})\\
\mathsf{float} & (\tau_1 = \mathsf{int}\land\tau_2=\mathsf{float})\lor(\tau_1=\mathsf{float}\land\tau_2=\mathsf{int})\\
\mathsf{error} & \mathrm{otherwise}
\end{cases}
$$

$$
\shdw \clash
$$

$$
\frac{prelude}{}
$$

$$
\frac{env\vdash a:\tau_1,Lv\quad env\vdash a:\tau_2,Rv\quad \sup()}{}
$$

## Expressions

$$
\frac{env\vdash_{RE} e_1:\tau_1\quad env\vdash e_2:\tau_2\quad \sup(\tau_1,\tau_2)<int}{env\vdash e_1*e_2 : int}
$$
