% 
% Game theory problem in MiniZinc.
% 
% From http://www.ms.uky.edu/~mathecon/ma416
% Vanderbei "Linear Programming - Foundations - and Extensions", 
% page 174ff
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

int: n = 3;
array[1..n, 1..n] of float: A; % payoff matrix
array[1..n] of var 0.0..1.0: X;   % probability of strategy j for column player
var -1.0..1.0: w;

% solve satisfy;
solve maximize w;
% solve :: float_search(X, 0.00001,input_order,indomain_split,complete) maximize w;


constraint 
        forall(i in 1..n) (
           sum(j in 1..n) (A[i,j]*X[j]-w) >=0.0
        )
;

% subject to probability:
constraint
    sum(j in 1..n) (X[j]) = 1.0
;

A = 
[|  0.0,  1.0, -2.0,
 | -3.0,  0.0,  4.0,
 |  5.0, -6.0,  0.0
 |];

output
[
  "X: " ++ show(X) ++ "\n" ++
  "w: " ++ show(w) ++ "\n" 
];