% 
% Five Element problem in MiniZinc.
% 
% From 
% Charles W. Trigg, PI MU EPSILON JOURNAL, Valume 6, Fall 1977, Number 5
% """
% From the following square array of the first 25 positive integers, 
% choose five, no two of the same row or column, so that the maximum of
% the five elements is as small as possible.
%
%    2 13 16 11 23
%   15  1  9  7 10
%   14 12 21 24  8
%    3 25 22 18  4
%   20 19  6  5 17
% """
% 
% Compare with the Formula One encoding
% http://www.f1compiler.com/samples/Five%20Elements.f1.html
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http:%www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;
array[1..n, 1..n] of int: matrix = 
   array2d(1..n, 1..n,
    [
    2, 13, 16, 11, 23,
   15,  1,  9,  7, 10,
   14, 12, 21, 24,  8,
    3, 25, 22, 18,  4,
   20, 19,  6,  5, 17,
   ]);        


% decision values
array[1..n, 1..n] of var 0..1: x;
array[1..n] of var 1..25: y;


solve :: int_search([x[i,j] | i,j in 1..n], input_order, indomain_min, complete) minimize max(y);

constraint

   forall(i in 1..n) (
      sum([x[i,j] | j in 1..n]) = 1 /\
      sum([x[j,i] | j in 1..n]) = 1
   )
   /\
   forall(j in 1..n) (
      exists(i in 1..n) (
        x[i,j] = 1 /\ 
        y[j] = matrix[i,j]
      )
   )
;


output 
[ "y: " ++ show(y) ++ " max y:" ++ show(max(y)) ++ "\n" ] 
++
[
  if j = 1 then "\n" else " " endif ++
    if fix(x[i,j]) = 1 then 
       show_int(2, matrix[i,j])
    else 
       "__"
    endif
  | i,j in 1..n
]
++ ["\n"] ++ 
[
  if j = 1 then "\n" else " " endif ++
    show_int(2, matrix[i,j])
  | i,j in 1..n
]
;


