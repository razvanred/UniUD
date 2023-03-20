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

% Input
start(0).

limit(6).

n(12).

strada(0,2,0).
strada(0,4,4).
strada(0,6,2).
strada(0,10,15).
strada(0,11,20).
strada(1,1,14).
strada(1,2,17).
strada(1,3,14).
strada(1,5,17).
strada(1,6,7).
strada(1,7,15).
strada(1,8,6).
strada(1,9,18).
strada(1,10,7).
strada(1,11,17).
strada(2,4,17).
strada(2,5,13).
strada(2,6,10).
strada(2,7,17).
strada(2,8,10).
strada(2,10,13).
strada(3,0,1).
strada(3,1,4).
strada(3,2,20).
strada(3,4,18).
strada(3,5,16).
strada(3,11,15).
strada(4,0,8).
strada(4,1,2).
strada(4,3,0).
strada(4,4,0).
strada(4,5,17).
strada(4,7,20).
strada(4,8,8).
strada(4,10,15).
strada(4,11,19).
strada(5,0,19).
strada(5,4,16).
strada(5,5,9).
strada(5,6,20).
strada(5,7,6).
strada(6,1,11).
strada(6,2,5).
strada(6,6,8).
strada(6,9,18).
strada(6,10,7).
strada(7,0,13).
strada(7,1,11).
strada(7,2,3).
strada(7,3,3).
strada(7,4,6).
strada(7,9,0).
strada(8,0,7).
strada(8,1,15).
strada(8,2,3).
strada(8,5,6).
strada(8,6,15).
strada(8,9,15).
strada(8,11,5).
strada(9,3,10).
strada(9,6,14).
strada(9,8,10).
strada(9,10,16).
strada(10,8,11).
strada(10,10,5).
strada(11,0,9).
strada(11,1,13).
strada(11,4,17).
strada(11,5,7).
strada(11,7,0).
strada(11,8,13).