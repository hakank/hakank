% 
% Coins problem in MiniZinc.
% 
% COINS PROBLEM:
% http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/coins_problem/coins_problem.pdf
% 
% This model is a port from the OPL version on page 4.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

set of int: coin = 1..6;
set of int: change = 1..100;
array[coin] of var 1..100: value;
array[coin] of var 1..100: num;
array[change, coin] of var 0..100: sel;

var int: sum_coins = sum(i in coin) (num[i]);

solve :: 
int_search(value ++ num ++ [sel[i,j] | i in change, j in coin] ++ [sum_coins], 
      first_fail, indomain_min, complete)  minimize sum_coins ;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

    forall(i in coin, s in change) (
         sel[s,i] <= num[i]
    )
    /\
    forall(s in change) (
         sum(i in coin) (value[i] * sel[s,i]) = s
    )
    /\ % symmetry-breaking
    forall(i in coin where i != 6) (
         value[i] * num[i] < value[i+1]
    )
;

output
[
  "value: " ++ show(value) ++ "\n" ++
  "sel: " ++ show(sel) ++ "\n" ++
  "sum_coins: " ++ show(sum_coins)
];