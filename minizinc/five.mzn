% 
% 5 x 5 puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 11.
% Description  : 5 X 5 puzzle
%  Source       : Unknown

% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s11.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 5;
set of 1..n: N = 1..n;
array[N, N] of var 0..1: x;
array[N,N] of var int:  d;

var int: moves = sum(i in N, j in N) (x[i,j]);

solve :: int_search([x[i,j] | i,j in N], first_fail, indomain_min, complete) minimize moves;

constraint 
 forall(i in N,j in N) (
        sum(k in j-1..j+1 where k >= 1 /\ k <= n /\ k != j) (x[i,k]) +
        sum(k in i-1..i+1 where k >= 1 /\ k <= n) (x[k,j]) = 2*d[i,j]+1
 )
;

output [
   if i = 1 /\ j = 1 then
    "\nmoves: " ++ show(moves)
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i in N, j in N
] ++ ["\n"];
