%
% Latin squares in Minizinc
%
% http://en.wikipedia.org/wiki/Latin_square:
% """
% A Latin square is an n × n table filled with n different symbols in such 
% a way that each symbol occurs exactly once in each row and exactly once 
% in each column. 
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 14;
array[1..n, 1..n] of var 1..n: x;

% solve satisfy;
solve :: int_search([ x[i, j] | i, j in 1..n], 
        first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 1..n) (
       all_different([ x[i, j] | j in 1..n]) /\
       all_different([ x[j, i] | j in 1..n])
   )
   /\ % symmetry breaking: first row is 1..n and first column is 1..n
      % i.e. the "reduced latin square"
   forall(i in 1..n) ( 
      x[1,i] = i /\
      x[i,1] = i
   )

;

output [
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];
