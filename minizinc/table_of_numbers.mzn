% 
% Table of numbers problem in MiniZinc.
% 
% From Fun With Num3ers
% "Table of numbers with 5 rows and 5 columns"
% http://benvitalenum3ers.wordpress.com/2012/03/15/table-of-numbers-with-5-rows-and-5-columns/
% """
% Below is a table of numbers with 5 rows and 5 columns. 
% From the table, choose 5 numbers in such a way that no two 
% numbers come from the same row and no two numbers come from the 
% same column. Find the sum of these 5 numbers. Explain the result.
%
%   7  8  5  9 10
%   6  7  4  8  9
%   9 10  7 11 12
%   8  9  6 10 11
%   4  5  2  6  7
%
% """

%
% There are 120 (=5!) different solutions to this problem.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;
array[1..n, 1..n] of int: t;

array[1..n, 1..n] of var 0..1: x;
var 0..sum([t[i,j] | i,j in 1..n]): the_sum;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 1..n) (
      sum([x[i,j] | j in 1..n]) = 1 /\
      sum([x[j,i] | j in 1..n]) = 1
   )

   /\
   the_sum = sum([x[i,j]*t[i,j] | i,j in 1..n])
;

output [
  "the_sum: " ++ show(the_sum) ++ "\n"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(t[i,j]*fix(x[i,j]))
  | i,j in 1..n
]
++ ["\n"]
;

%
% data
%
t = array2d(1..n, 1..n,
[
7,  8,  5,  9, 10,
6,  7,  4,  8,  9,
9, 10,  7, 11, 12,
8,  9,  6, 10, 11,
4,  5,  2,  6,  7
]);