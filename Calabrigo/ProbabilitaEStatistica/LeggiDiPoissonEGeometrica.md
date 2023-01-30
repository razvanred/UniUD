# Legge di Poisson
La legge di poisson serve per trovare il numero di eventi che accadono nell'unità di tempo o di spazio. La formula è `P(λ) = e^-λ * (λ^x)/x!`, e si scrive P(λ). E(X) = VAR(X) = λ.

Esempio: Mi arrivano 2 chiamate al minuto, qual'è la probabilità che io riceva 5 chiamate nei prossimi 2 minuti?\
X="# chiamate nel prox minuto"\
λ = 2*2 = 4\
p(10) = e^-4 * (4^5)/5! = 0.156 = 15.6%

# Legge Geometrica
La legge geometrica serve per trovare la probabilità che un evento A accada all'n-esimo tentativo. La formula è `P(p) = p*(1-p)^(x-1)`, e si scrive P(p).\
Si dice che la legge Geometrica non abbia memoria, perchè P(A|B) = P(A), se A="sono uscite 90 teste", B="sono uscite 187 teste"; allora P(B|A) = P(B) e P(A|B) = P(A), perchè i 2 esperimenti non si influenzano in alcun modo.

Qual'è la probabilità di ottenere testa all'ennesimo lancio di una moneta?\
X="testa all'11-esimo lancio della moneta sbilanciata con T=0.3 e C=0.7"\
P(X) ~ GE(p) = GE(0.3) = 0.3*(0.7)^(11-1) = 0.85%