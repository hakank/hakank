% 
% Enigma 248: Add or multiply in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2015/01/05/enigma-248-add-or-multiply/
% """
% From New Scientist #1395, 2nd February 1984
% I gave each of the five boys a different amount of money, and said to each:
%
%   "Spend all this on goodies at the corner shop. Each goodie you buy must cost a 
%    different exact number of half-pennies. When you multiply together the prices 
%    (in pence) of all your purchases, the result must be the same as if you 
%   added them."
%
% When they came back, they had all done as I asked: each had bought the same 
% number of goodies; and in one case each of the prices was an exact 
% whole number of pence.
%
% How much did my generosity cost me altogether?
% """

% This is the unique solution:
%
%  total: [6.0, 7.5, 9.0, 10.5, 15.0]
%  number of half-pennies:
%  [2, 4, 6]
%  [2, 3, 10]
%  [1, 8, 9]
%  [1, 6, 14]
%  [1, 5, 24]
% 
%  pennies:
%  [1.0, 2.0, 3.0] total: 6.0
%  [1.0, 1.5, 5.0] total: 7.5
%  [0.5, 4.0, 4.5] total: 9.0
%  [0.5, 3.0, 7.0] total: 10.5
%  [0.5, 2.5, 12.0] total: 15.0
%
%  sum_total: 48.0
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5; % number of boys
int: m = 3; % number of goodies

array[1..n] of var float: total;
array[1..n,1..m] of var 1..30: x; % number of half-pennies per goodie

% solve satisfy;
solve :: int_search(array1d(x), first_fail, indomain_split, complete) satisfy;

constraint
  forall(i in 1..n) (
     % "When you multiply together the prices (in pence) of all your purchases, 
     % the result must be the same as if you added them."
     let {
       array[1..m] of var float: t = [x[i,j] * 0.5 | j in 1..m]
     } in
     total[i] = sum(t) /\
     total[i] = product(t) 
     /\ total[i] > 0
     /\ 
     % "Each goodie you buy must cost a different exact number of half-pennies."
     % (the != and <= construct is needed for some float solvers)
     forall(j in 2..m) (
       t[j-1] != t[j] /\
       t[j-1] <= t[j] 
     )
  )
  /\ % "in one case each of the prices was an exact whole number of pence."
  sum(i in 1..n) ( sum(j in 1..m) ( x[i,j] mod 2 = 0 ) = m  ) = 1

  /\ % symmetry breaking
  forall(i in 2..n) (
    total[i-1] != total[i] /\
    total[i-1] <= total[i]
  )
;

output 
[
  "number of half-pennies:\n",
]
++
[
  show([x[i,j] | j in 1..m]) ++ "\n"
  | i in 1..n
]
++ ["\npennies:\n"] ++
[
  show([x[i,j] *0.5 | j in 1..m]) ++ " total: " ++ show(total[i]) ++ "\n"
  | i in 1..n
]
++
[ 
 "total: ", show(total), "\n",
 "sum_total: ", show(sum(total)), "\n",
]
;

