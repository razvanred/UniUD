% Elenco macchine
car(a).
car(b).

% Elenco nodi
node(0..N-1) :- n(N).

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

% Non possono essere allo stesso nodo a tempi troppo vicini
:- at(a,X,T1), at(b,X,T2), node(X), time(T1), time(T2), limit(L), |T1-T2| <= L.

% Input
start(0).

limit(6).

n(5).

strada(0,1,10).
strada(0,3,6).
strada(1,0,12).
strada(1,4,16).
strada(2,1,1).
strada(2,3,0).
strada(2,4,12).
strada(3,4,0).
strada(4,2,0).