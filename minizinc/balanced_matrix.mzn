% 
% Balanced 0-1 matrix in MiniZinc.
% 
% See
% http://en.wikipedia.org/wiki/Dynamic_programming
% A type of balanced 0-1 matrix
% """
% Consider the problem of assigning values, either zero or one, to the positions 
% of an n x n matrix, n even, so that each row and each column contains 
% exactly n/2 zeros and n/2 ones.
% """
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 6; % must be even
int: m = n div 2;

array[1..n, 1..n] of var 0..1: x;


solve satisfy;

constraint
   assert(n mod 2 = 0, "n must be even")
   /\
   forall(i in 1..n) (
        sum(j in 1..n) (bool2int(x[i,j]=1)) = m
   )
   /\
   forall(j in 1..n) (
        sum(i in 1..n) (bool2int(x[i,j]=1)) = m
   )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];