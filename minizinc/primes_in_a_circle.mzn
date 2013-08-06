% 
% Primes in a circle in MiniZinc.
% 
% From 
% http://wordplay.blogs.nytimes.com/2013/06/17/primes-circle/?_r=0
% """
% This week’s puzzle was suggested by University of the 
% Andes Professor Bernardo Recamán.
%
% Place the numbers 1 to 14 around a circle so that both the sum and 
% the (positive difference) of any two neighboring numbers is a prime.
% """
% 
% There are 28 solutions. With the symmetry breaking (x[1] = 1)
% there are two solutions:
%    [1, 12, 5, 2, 9, 14, 3, 8, 11, 6, 13, 10, 7, 4]
%    [1, 4, 7, 10, 13, 6, 11, 8, 3, 14, 9, 2, 5, 12]
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 14;
set of int: primes = {2,3,5,7,11,13,17,19,23};

% decision variables 
array[1..n] of var 1..14: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

predicate p(var int: a, var int: b) = 
      a + b in primes
      /\
      abs(a - b) in primes

;

constraint
   alldifferent(x) 
   /\
   forall(i in 1..n-1) (
      p(x[i],x[i+1])
   )
   /\
   p(x[1],x[n])

   /\ % symmetry breaking
   x[1] = 1
;

output [
  show(x)
];

