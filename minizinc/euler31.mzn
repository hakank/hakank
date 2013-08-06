% 
% Project Euler problem 31 in MiniZinc.
% 
% http://projecteuler.net/problem=31
% """
% In England the currency is made up of pound, L, and pence, p, and there are eight coins in general circulation:
%
%     1p, 2p, 5p, 10p, 20p, 50p, L1 (100p) and L2 (200p).
%
% It is possible to make £2 in the following way:
%
%     1×L1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
%
% How many different ways can L2 be made using any number of coins?

% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 8;
array[1..n] of int: coins = [200,100,50,20,10,5,2,1];

% decision variables
array[1..n] of var 0..200: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  sum(i in 1..n) (coins[i]*x[i]) == 200
;

output [
  show(x)
];

