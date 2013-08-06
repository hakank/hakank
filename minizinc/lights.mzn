% 
% Lights on puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 12.
% Description  : Lights on puzzle
% Source       : Unknown

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s12.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: n = 4;

set of 1..n: N = 1..n;
array[N, N] of int: r;
array[N, N] of var 0..1: x; 
array[N, N] of var int: d; %  >= 0;

var int: moves = sum(i in N, j in N) (x[i,j]);

solve :: int_search([x[i,j] | i,j in N], first_fail, indomain_min, complete) minimize moves;


constraint
  forall(i, j in N) (
    d[i,j] >= 0
  )
  /\
  forall(i in N, j in N) (
        sum(l in N) (x[i,l]) +
           sum(k in N where k != i) (x[k,j]) = 2*d[i,j]+r[i,j]
  )
;

r = 
[|0, 1, 0, 0,
 |1, 0, 1, 0,
 |1, 1, 0, 0,
 |0, 1, 1, 1
|]
;


output [
  if i = 1 /\ j = 1 then 
   "\nmoves: " ++ show(moves)
  else "" endif ++
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i in N, j in N
] ++ ["\n"];
