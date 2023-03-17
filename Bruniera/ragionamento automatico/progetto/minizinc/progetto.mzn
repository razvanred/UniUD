include "globals.mzn";

par int: start;
par int: limit;

par int: n;

array [1..n,1..n] of par int: m;

              
array [1..2,1..n+1] of var 1..n: path;

array [1..2,1..n] of var int: time;

% Inizio e fine
constraint path[1,1] = start /\
           path[2,1] = start /\
           path[1,n+1] = start /\
           path[2,n+1] = start;

% Inizi diversi (ridondante)
constraint path[1,2] != path[2,2];

% Simmetry break
constraint path[1,2] > path[2,2];

% Attraversa tutti gli incroci una sola volta
constraint all_different (i in 1..n) (path[1,i]);
constraint all_different (i in 1..n) (path[2,i]);

% Gli archi con pesi negativi sono assenti
constraint forall (i in 1..n) (m[path[1,i],path[1,i+1]] >= 0);
constraint forall (i in 1..n) (m[path[2,i],path[2,i+1]] >= 0);

% Popolazione dell'array di tempi (accesso random)
constraint forall (i in 1..n)
              (time[1,path[1,i]] = sum (j in 1..i-1) 
                  (m[path[1,j],path[1,j+1]]));
constraint forall (i in 1..n)
              (time[2,path[2,i]] = sum (j in 1..i-1)
                  (m[path[2,j],path[2,j+1]]));
              
% Popolazione dei tempi ridondante (incrementale)
constraint time[1,1] = 0 /\ time[2,1] = 0;
constraint forall (i in 2..n)
              (time[1,path[1,i]] = time[1,path[1,i-1]] + m[path[1,i-1],path[1,i]]);
constraint forall (i in 2..n)
              (time[2,path[2,i]] = time[2,path[2,i-1]] + m[path[2,i-1],path[2,i]]);

% Non passano per lo stesso incrocio allo stesso tempo
constraint forall (i in 2..n) (abs(time[1,i]-time[2,i]) > limit);

% Constraint ridondante, tutti i 

% Minimizza il tempo
var int: total = (sum (i in 1..n) (m[path[1,i],path[1,i+1]])) +
               (sum (i in 1..n) (m[path[2,i],path[2,i+1]]));

% Massimizza la la differenza
var int: max_difference = min (i in 2..n) (abs(time[1,i]-time[2,i]));

solve satisfy;
%solve minimize total;
%solve maximize max_difference;

%output ["path \(path)\ntime \(time)\ntotal \(total)"];