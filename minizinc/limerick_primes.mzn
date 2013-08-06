% 
% Limerick primes in MiniZinc.
% 
% From John D. Cook: Limerick Primes
% http://www.johndcook.com/blog/2011/03/08/limerick-primes
% """
% The other day, Futility Closet posted this observation:
%
%     10102323454577 is the smallest 14-digit prime number that 
%     follows the rhyme scheme of a Shakespearean sonnet 
%     (ababcdcdefefgg).
%
% I posted this on AlgebraFact and got a lot of responses. One was from 
% Matt Parker who replied that 11551 was the smallest prime with a limerick 
% rhyme scheme.
% 
% So how many limerick primes are there? Well, there aren’t many candidates. 
% A limerick prime has to have the form AABBA where A is an odd digit and B 
% is any digit other than A. So for each of five choices of A, there are 
% nine possible B’s. 
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;

var 10000..99999: p;
array[1..n] of var 0..9: x;


predicate is_prime(var int: x) =
   x > 1 
   /\
   forall(i in 2..1+ceil(sqrt(int2float(ub(x))))) ( 
        (i < x) -> (x mod i > 0)
   )
;

% channel array a <-> var int n
predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   is_prime(p) /\ toNum10(x, p)
;

% """
% A limerick prime has to have the form AABBA where A is an 
% odd digit and B is any digit other than A. 
% """
constraint
   x[1] mod 2 = 1
   /\ % AA..A
   x[1] = x[2] /\ x[1] = x[5]
   /\ % ..BB.
   x[3] = x[4]
   /\ 
   x[1] != x[3]
;

output [
  "p: " ++ show(p) ++ "\n" ++
  "x: " ++ show(x)
]
 ++ ["\n"]
;

