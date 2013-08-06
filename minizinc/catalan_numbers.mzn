% 
% Catalan numbers in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Catalan_number
% """
% C[0] = 1 
% C[n+1]= sum(i in 0 .. n) (C[i]*C[n-i])
% """
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

% int: m = 10; % UNSAT due to integer overflow for several solvers.
int: m = 7; % Should be OK for most solvers
array[0..m] of var 0..1000: C;


% solve satisfy;
solve :: int_search(C, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i in 0..m) (
     C[i] >= 0
  )
  /\
  C[0] = 1
  /\
  forall(n in 0..m-1) (
    C[n+1] = sum(j in 0..n) (C[j]*C[n-j])
  )
;

output [
  "C: ", show(C)
];
