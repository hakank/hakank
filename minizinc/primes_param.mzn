% 
% Generating prime numbers as parameters in MiniZinc.
% 
% This is model to show how to make primes as a set of param int.
%
% Including a simple CSP: the sum of n different primes must be a prime
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;
int: max_val = 1000;

% note: need to add {2}
% Not very efficent...
set of int: primes = {2} union (2..max_val diff { i | i in 2..max_val, j in 2..ceil(sqrt(i))+1 where i mod j = 0});
% slower:
% set of int: primes = 2..n diff { i | i in 2..n, j in 2..i div 2 where i mod j = 0};

array[1..n] of var primes: x; % only work with primes
var primes: z = sum(x); % sum must be a prime (in range 2..n)

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  trace("primes: " ++ show(primes) ++ "\n" ++
         "card: " ++ show(card(primes)) ++ "\n", true)
;

constraint
  alldifferent(x) % :: domain
  /\
  increasing(x) 
;


output [
  "x: " ++ show(x) ++ "  " ++ "z: " ++ show(z) ++ "\n"
  %  ++ "primes: " ++ show(primes) ++ "\n"  % with this printed it's quite slow
];

