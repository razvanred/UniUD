# Algebra

## Affine Spaces

An affine space is a triple $\lang A,V,+\rang$ where $A$ is a set (of points), $V$ is a vector space, and $+:A\times V\rightarrow A$ is a function satisfying the following conditions:

1. $P+0=P\quad\forall P\in A$,$\quad0\in V$
2. $(P+u)+v=P+(u+v)$,$\quad\forall P\in A$,$\quad\forall u,v\in V$
3. $\forall P,Q\in A$,$\quad \exists u:P+u=Q$

The unique vector $u:P+u=Q$ is denoted by $\overrightarrow{PQ}$ or $Q-P$. We have then $P+\overrightarrow{PQ}=P+(Q-P)=Q$.

Addition of points does not exist.

### Affine Subspace

The affine subspaces $\lang S,W,+\rang$ of $A$ are the subsets in the form $S=P+W=\{P+w\mid w\in W\}\subseteq A$, where $P$ is a fixed point of $A$ and $W$ is a subspace of $V$. This represents all the points $P$ can be translated to via $W$, thus the dimension of $S$ is $\mathrm{dim}(W)$.

### Affine Independence

The points $P_0,\ldots,P_k$ of an affine space are affinely independent if and only if, the vectors $\overrightarrow{P_0P_1},\ldots,\overrightarrow{P_0P_k}$ are linearly independent $\forall P_0\in A$.\
$k$ points in an affine space are affinely independent iff the smallest affine subspace containing them has dimension $k-1$.

* 2 points are affinely independent if and only if they are distinct
* 3 points are affinely independent if and only if they are not collinear
* 4 points are affinely independent if and only if they are not coplanar

### Affine Combination, Convex Combination

Given two points $P$ and $Q$ and a third point $P_0$.

The affine combination of $P$ and $Q$ is defined as $P'=\alpha(P-P_0)+\beta(Q-P_0)+P_0$ with $\alpha+\beta=1$. This definition is independent of $P_0$ iff $\alpha+\beta=1$:

$$
P'=\alpha(P-P_0)+\beta(Q-P_0)+P_0=\alpha P+\beta Q-\alpha P_0-\beta P_0+P_0=\alpha P+\beta Q
$$

(this is a stretch of notation) Can be extended to $n$ points:

* the affine combinations of 2 independent points describe the line containing them
* the affine combinations of 3 independent points describe the plane containing them

Convex combinations of $n$ points $\alpha_n(P_n-P_0)$ are affine with $\alpha_n\geq0$:

* The convex combinations of 2 independent points describe the segment joining them
* The convex combinations of 3 independent points describe the triangle that has the points as vertices

## Linear and Affine Transformations

### Linear Transformations

Let $V$ be a vector space over the field $K$. A transformation (mapping) $f:V\rightarrow V$ is linear iff it preserves linear combination:

* $f(\alpha x)=\alpha f(x)$,$\quad\forall\alpha\in K, x\in V$
* $f(x+y)=f(x)+f(y)$,$\quad\forall x,y\in V$

A linear transformation is orientation-preserving iff it preserves the direction of angles (clockwise/counterclockwise).

### Rigid Linear Transformations

A linear transformation is rigid iff it preserves the dot product $x\cdot y=f(x)\cdot f(y)$, that is, if it preserves the angles and the norms.

The linear transformation represented by matrix $M$ is rigid iff $M^{-1}=M^t$. Such matrices are called orthogonal

Every linear, rigid, orientation-preserving transformation is a rotation and viceversa.

### Affine Transformations

Let $\lang A,V,+\rang$ be an affine space. A transformation is affine iff it preserves affine combination (of points).

Every affine map is determined by the image of any point plus a linear map. For any affine map $f:A\rightarrow A$ there is a point $P_0$ and a unique linear map $h:V\rightarrow V$ so that:

$$
f(P)=f(P_0+v)=f(P_0)+h(v)=f(P_0)+h(P-P_0)\\
\forall P\in A
$$

In general:

$$
f(P)-P_0=(f(P_0)-P_0)+P-P_0\\
f(x)=Hx+t
$$

Affine maps for which $h$ is the identity are called translations: $f(P)=P+(f(P_0)-P_0)$.

Every rigid, orientation-preserving transformation of $\mathbb{R}^n$ is affine and can be uniquely expressed as the composition of a translation and a rotation. Translations are rigid.

$$
f(x)=Rx+t
$$

Note that $RR^t=I$.

---

## Homogeneous Coordinates

Homogeneous coordinates have the property that the tuples $(x_1,x_2,…,1)$  and $(αx_1,αx_2,\ldots,α)$ represent the same point (for $α\neq0$). Thus, unlike cartesian coordinates, a single point can be represented by infinitely many homogeneous coordinates.

By using homogeneous coordinates, you move from the euclidean space $\mathbb{R}^{n+1}$ to the projective space $\mathbb{P}^n(\mathbb{R})$ by identifying points in $\mathbb{R}^{n+1}$ that lie on the same line through the origin (minus the origin itself) with each other. All of the points that have the same projection are an equivalence class corresponding to a single point in the projective space.

### Affine transformations in homogeneous coordinates

We can use homogeneous coordinates to both implement affine transformations and mark the distinction between points and vectors. We add a $n+1$ component to $A^n$, which will be 1 for points and 0 for vectors. This brings various benefits:

* point $+$ vector $=$ point
* point $-$ point $=$ vector
* $\alpha P+\beta Q=$ point
* affine tranformations encoding

Affine transformations of points $f(P)=h(P-P_0)+f(P_0)$ and vectors $f(x)=Hx+t$ become respectively:

$$
\begin{pmatrix}
y\\
1
\end{pmatrix}
=
\begin{pmatrix}
H & t\\
0 & 1
\end{pmatrix}
\begin{pmatrix}
x\\
1
\end{pmatrix}\\
\begin{pmatrix}
y\\
0
\end{pmatrix}
=
\begin{pmatrix}
H & t\\
0 & 1
\end{pmatrix}
\begin{pmatrix}
x\\
0
\end{pmatrix}
$$

Note that vectors are unaffected by translation.

An affine map of $\mathbb{R}^n$ becomes a linear map of $\mathbb{R}^{n+1}$
