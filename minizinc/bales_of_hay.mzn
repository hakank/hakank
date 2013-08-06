% 
% Bales of hay problem in MiniZinc.
% 
% From The Math Less Traveled, 
% "The haybaler", http://www.mathlesstraveled.com/?p=582 
% """
% You have five bales of hay.
%
% For some reason, instead of being weighed individually, they were weighed 
% in all possible combinations of two. The weights of each of these 
% combinations were written down and arranged in numerical order, without 
% keeping track of which weight matched which pair of bales. The weights, 
% in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.
%
% How much does each bale weigh? Is there a solution? Are there multiple 
% possible solutions? 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: n = 5;
array[1..n] of var 0..50: bales;

array[1..10] of int: weights = [80, 82, 83, 84, 85, 86, 87, 88, 90, 91];

% solve satisfy;
solve :: int_search(bales, occurrence, indomain_min, complete) satisfy;

constraint
   forall(w in 1..10) (
     let {
        var 1..n: i,
        var 1..n: j
     } in
       i < j /\
       bales[i] + bales[j] = weights[w]
   )
   /\ increasing(bales)
;


output [
  % "bales: " ++ show(bales) ++ "\n"
  "bale" ++ show(i) ++ ": " ++ show(bales[i]) ++ "\n"
  | i in 1..n
] ++ ["\n"];