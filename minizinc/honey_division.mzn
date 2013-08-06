% 
% Honey division puzzle in MiniZinc.
% 
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 6.
% Description  : Honey division puzzle
% Source       : H E Dudeney - Amusements in Mathematics

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s6.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: son = 3;
int: cap = 3;

set of 1..son: S = 1..son;
set of 1..cap: C = 1..cap;
array[C] of var int: howfull;
array[S, C] of var 1..4: x;

var int: sumx = sum(i in S, j in C) (x[i,j]);

solve satisfy;

constraint 
   %  each son gets 7 barrels
   forall(i in S) (
        sum(j in C) (x[i,j]) = 7
   )
   /\
   %  each son gets 3.5 units
   forall(i in S) (
     % multiplies with 2 for the integer version
     sum(j in C) ( howfull[j]*x[i,j]) = 7
   )
   /\
   %  use 7 of each barrel capacity
   forall(j in C) (
      sum(i in S) (x[i,j]) = 7
   )
;

% howfull = [1, 0.5, 0]
howfull = [2, 1, 0]; % multiplies with 2 for the integer version


output [
   "sumx: ", show(sumx)
] ++
[
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in S, j in C
] ++ ["\n"];

