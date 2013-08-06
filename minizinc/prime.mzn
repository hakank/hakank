%
% Simple number theory in Minizinc
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 33;

var 0..n: num_divisors;
int: t = (n div 2)+1; % number of elements in the divisors array
array[2..t] of var 0..n: divisor; 
var 0..1: is_prime;

%
% Calculate the number of divisors of n
%
predicate divisors(int: n, var int: num_divisors, array[int] of var int: d) =
   let { 
         int: t = (n div 2)+1
         } 
   in 
    % is a divisor?
    forall(i in 2..t) (
        (d[i] = 1 <-> n mod i = 0) /\
        (d[i] = 0 <-> n mod i > 0)
     ) 
    /\ 
    num_divisors = sum(i in 2..t) (bool2int(d[i]>0))
;

% solve satisfy;
solve :: int_search(divisor, first_fail, indomain_min, complete) satisfy;

constraint
   divisors(n, num_divisors, divisor)
   /\
   (is_prime = 1 <-> num_divisors = 0) /\
   (is_prime = 0 <-> num_divisors > 0)
;


output
[
  "n: " ++ show(n) ++ "\n" ++
  "divisor: " ++ show(divisor) ++ "\n" ++
  "is_prime: " ++ show(is_prime) ++ "\n" 
];