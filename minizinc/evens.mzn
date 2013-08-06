% 
% Evens puzzle in MiniZinc.

% From Martin Chlond Integer Programming Puzzles:

% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 4. 
% Description  : Evens puzzle
% Source       : Boris Kordemsky - The Moscow Puzzles (P8)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s4.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: square = 4;
int: coin = 10;
 
set of 1..square: S = 1..square;
array[S,S] of var 0..1: x;
array[S] of var int: n;
array[S] of var int: m;

solve satisfy;
  
constraint
    sum(i in S,j in S) (x[i,j]) = coin
    /\
    forall(i in S) (
        sum(j in S) (x[i,j]) = 2*n[i]
    )
   /\
   forall(j in S) (
        sum(i in S) (x[i,j]) = 2*m[j]
   )
;


output [
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in S, j in S
] ++ ["\n"];