% 
% Smuggler's knapsack problem in MiniZinc.
% 
% Marriott & Stuckey: 'Programming with constraints', page  101f, 115f
%
% Smuggler's knapsack.
% 
% A smuggler has a knapsack with a capacity of 9 units.
%             Unit       Profit
% Whisky:     4 units    15 dollars
% Perfume:    3 units    10 dollars
% Cigarettes: 2 units     7 dollars
%
% What is the optimal choice?
%
% Compare with the following models:
% * ECLiPSe: http://www.hakank.org/eclipse/smuggler_knapsack.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/smuggler_knapsack.pl
% * Gecode: http://www.hakank.org/gecode/smuggler_knapsack.cpp
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
int: c = 9;

array[1..n] of int: units = [4,3,2];
array[1..n] of int: profits = [15,10,7];

% decision variables
array[1..n] of var 0..c: x;
var int: TotalProfit = sum(i in 1..n) (x[i]*profits[i]);

solve :: int_search(x, first_fail, indomain_max, complete) maximize TotalProfit;

constraint

  sum(i in 1..n) (x[i]*units[i]) <= c
    
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "TotalProfit: " ++ show(TotalProfit) ++ "\n"
];