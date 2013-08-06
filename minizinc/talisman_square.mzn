%
% Talisman Square in MiniZinc.
% 
% http://mathworld.wolfram.com/TalismanSquare.html
% """
% An n×n array  of the integers from 1 to n^2 such that the difference between 
% any one integer and its neighbor (horizontally, vertically, or diagonally, without 
% wrapping around) is greater than or equal to some value k is called a (n,k)-talisman 
% square. 
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 5;
int: k = 2;
array[1..n, 1..n] of var 1..n*n: x;

solve :: int_search([x[i,j] | i, j in 1..n], first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
   all_different([x[i,j] | i, j in 1..n]) 
   /\
   forall(i, j in 2..n) (
     abs(x[i,j]-x[i-1,j]) >= k /\
     abs(x[i,j]-x[i,j-1]) >= k
   )
   /\
   forall(i, j in 1..n-1) (
     abs(x[i,j]-x[i+1,j]) >= k /\
     abs(x[i,j]-x[i,j+1]) >= k
   )
   % /\ 
   % % some symmetry breaking
   % x[1,1] = 1

;


output 
[
  "["
]
++
[
  if j = 1 then ",\n" else "," endif ++
    show_int(2, x[i,j])
  | i,j in 1..n
] ++ ["],\n"];
