% Macchine
car(a).
car(b).

% Nodi
node(X) :- strada(X,Y,Z).
% I nodi sono N, se ne manca qualcuno è perché non ha nodi uscenti ed è insoddisfacibile
N{node(X) : node(X)}N :- n(N).

% Ci sono N+1 passaggi
arc(X) :- node(X).
arc(X) :- n(X).

% Iniziano e finiscono in start
step(Z,0,X) :- start(X), car(Z).
step(Z,Y,X) :- start(X), car(Z), n(Y).
% C'è esattamente uno step per ogni arco
1{step(Z,Y,X) : arc(Y), node(X)}1 :- car(Z), arc(Y).
% C'è esattamente uno step per ogni nodo (tranne il nodo start)
1{step(Z,Y,X) : arc(Y), arc(Y)}1 :- car(Z), node(X), X != S, start(S).
% Iniziano con step diversi (ridondante)
:- step(a,1,X), step(b,1,X).

% Due nodi sono o connessi o non connessi
con(X,Y) :- strada(X,Y,Z).
1{strada(X,Y,Z) : strada(X,Y,Z)}1 :- con(X,Y).
:- strada(X,Y,Z), ncon(X,Y).
1{con(X,Y); ncon(X,Y)}1 :- node(X), node(Y).

% Gli step devono essere legali
:- step(Z,T,X), step(Z,T+1,Y), ncon(X,Y), car(Z), arc(T).


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