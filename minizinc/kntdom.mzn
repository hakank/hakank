% 
% Knight domination puzzle in MiniZinc.
% 
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 8. 
% Description  : Knight domination puzzle - all squares threatened
% Source       : M Kraitchik - Mathematical Recreations (P256)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s8.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: rows = 8;
int: cols = 8;
  
array[1..rows+4,1..cols+4] of var 0..1: x;
var int: minnum = sum(i in 3..rows+2,j in 3..cols+2) (x[i,j]);

solve :: int_search([x[i,j] | i in 1..rows+4,j in 1..cols+4], 
        first_fail, indomain_min, complete) minimize minnum;

constraint 
 % Every real square threatened 
 forall(i in 3..rows+2,j in 3..cols+2) (
        x[i-2,j-1]+x[i-1,j-2]+x[i+1,j-2]+x[i+2,j-1]+
        x[i+2,j+1]+x[i+1,j+2]+x[i-1,j+2]+x[i-2,j+1] >= 1
  )
  /\ % Dummy squares not occupied 
  sum(i in 1..2,j in 1..cols+4) (x[i,j])+
  sum(i in rows+3..rows+4,j in 1..cols+4) (x[i,j]) +
  sum(j in 1..2,i in 3..rows+2) (x[i,j]) +
  sum(j in rows+3..rows+4,i in 3..rows+2) (x[i,j]) = 0

;

output [
  if i = 3 /\ j = 3 then 
   "\nminnum: " ++ show(minnum)
  else "" endif ++
  if j = 3 then "\n" else " " endif ++
  show(x[i, j])  
  | i in 3..rows+2, j in 3..cols+2
] ++ ["\n"];
