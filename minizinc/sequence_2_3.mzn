% 
% Sequence puzzle in Minizinc.
% 
% From the ECLiPSe book "Constraint Logic Programming using ECLiPSe", 
% page 109. Exercise 6.2
% """
%  Find a permutation of numbers 1,..,10 such that
%  - 6 is put on 7th place
%  - each number starting from the second one is either bigger by 3 or smaller
%    by 2 than its predecessor.
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

array[1..10] of var 1..10: x;

solve satisfy;

constraint
   all_different(x) /\      
   x[6] = 7 /\
   forall(i in 2..10) (
      x[i] - x[i-1] = 3 \/
      x[i] - x[i-1] = -2 
   )
;

output [
  show(x), "\n",
];