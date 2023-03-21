% Elenco macchine
car(a).
car(b).

% Elenco nodi
node(0..N-1) :- n(N).

% Elenco tempi
% Se non si limitano i tempi non finisce mai il grounding
time(0..T) :- T = #sum{L : strada(X,Y,L)}.

% Se esiste una strada *può* andare da X a Y
{go(Z,X,Y)} :- strada(X,Y,_), car(Z).

% Se vai da un posto visitato ad un altro, lo visiti
seen(Z,X) :- go(Z,S,X), start(S), car(Z).
seen(Z,Y) :- seen(Z,X), go(Z,X,Y), car(Z).

% Vai dappertutto una volta sola
1{go(Z,X,Y) : strada(X,Y,_)}1 :- node(X), car(Z).
1{go(Z,X,Y) : strada(X,Y,_)}1 :- node(Y), car(Z).
:- node(X), not seen(Z,X), car(Z).

% Se era al nodo X al tempo T sarà al nodo Y al tempo T+L
at(Z,X,T) :- go(Z,S,X), start(S), car(Z), strada(S,X,T), time(T).
at(Z,Y,T+L) :- at(Z,X,T), go(Z,X,Y), car(Z), strada(X,Y,L), X != S, start(S), time(T), time(L).

% Usare questa invece del "not seen" è più lento
%:- node(X), car(Z), 1{not at(Z,X,T) : time(T)}1.

% Non possono essere allo stesso nodo (eccetto S) a tempi troppo vicini
:- at(a,X,T1), at(b,X,T2), node(X), time(T1), time(T2), limit(L), |T1-T2| <= L, x != S, start(S).

% Questo symmetry break peggiora
%:- go(a,S,X), go(a,S,Y), start(S), X > Y.

% Input mancante