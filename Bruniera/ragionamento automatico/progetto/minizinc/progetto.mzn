include "globals.mzn";

par int: start;
par int: limit;

par int: n;

array [1..n,1..n] of par int: m;

int: max_time = sum (i,j in 1..n) (if m[i,j] > 0 then m[i,j] else 0 endif);

% Variabili          
array [1..2,1..n+1] of var 1..n: path;

array [1..2,1..n] of var 0..max_time: time ::is_defined_var; %::mzn_check_var;

% Inizio e fine
constraint path[1,1] = start /\
           path[2,1] = start /\
           path[1,n+1] = start /\
           path[2,n+1] = start ::domain;

% Inizi diversi (ridondante)
%constraint path[1,2] != path[2,2];

% Limita i tempi
%constraint forall (i in 1..n)
%               (time[1,i] in 0..max_time /\
%                time[2,i] in 0..max_time);

% Simmetry break
% annotarlo come symmetry_breaking_constraint peggiora i risultati
constraint path[1,2] > path[2,2];

% Attraversa tutti gli incroci una sola volta
constraint all_different (i in 1..n) (path[1,i]) ::domain;
constraint all_different (i in 1..n) (path[2,i]) ::domain;

% Gli archi con pesi negativi sono assenti
constraint forall (i in 1..n) (m[path[1,i],path[1,i+1]] >= 0);
constraint forall (i in 1..n) (m[path[2,i],path[2,i+1]] >= 0);

% Popolazione dell'array di tempi (accesso random)
%constraint forall (i in 1..n)
%              (time[1,path[1,i]] = sum (j in 1..i-1) 
%                  (m[path[1,j],path[1,j+1]]));
%constraint forall (i in 1..n)
%              (time[2,path[2,i]] = sum (j in 1..i-1)
%                  (m[path[2,j],path[2,j+1]]));
              
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
               (sum (i in 1..n) (m[path[2,i],path[2,i+1]])) ::is_defined_var;

% Massimizza la la differenza
%var int: min_difference = min (i in 2..n) (abs(time[1,i]-time[2,i]));

%solve satisfy;
solve 
  ::seq_search([relax_and_reconstruct([path[j,i] | i in 1..n+1, j in 1..2], 50),int_search(path, input_order, indomain_split, complete)])
  %::int_search(path, input_order, indomain_reverse_split, complete)
  %::restart_linear(n)
  ::restart_geometric(1.5, n)
  minimize total;
%solve maximize min_difference;

output ["path: \([path[1,i] | i in 1..n+1])\n      \([path[2,i] | i in 1..n+1])\ntime: \([time[1,i] | i in 1..n])\n      \([time[1,i] | i in 1..n])\ntotal \(total)"];