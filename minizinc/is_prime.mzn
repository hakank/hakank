% 
% Primes in MiniZinc.
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 100;

% array[1..n] of var 0..1: x;

var set of 1..n: primes;
var 0..n: num_primes = card(primes);

predicate is_prime(var int: x) =
   x > 1 
   /\
   forall(i in 2..1+ceil(sqrt(int2float(ub(x))))) ( 
        (i < x) -> (x mod i > 0)
   )
;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  % x = 13
  % /\
  % is_prime(x)
  forall(i in 1..n) (
     %(x[i] = 1 <-> is_prime(i))
     %/\
     (i in primes <-> is_prime(i))
  )
;

output [
 % "x: " ++ show(x) ++ "\n" ++
 "primes: " ++ show(primes) ++ "\n" ++
 "num_primes:" ++ show(num_primes)
] ++ ["\n"]

