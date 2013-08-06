% 
% Remainder puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 10.
% Description  : Remainder puzzle
% Source       : Boris Kordemsky - The Moscow Puzzles (P136)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s10.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

array[1..4] of var int: x;
var int: n;

solve minimize n;

constraint
   forall(i in 1..4) (x[i] >= 1)  /\
   n >= 1 /\
   -3*x[1] + n = 1 /\
   -4*x[2] + n = 2 /\
   -5*x[3] + n = 3 /\
   -6*x[4] + n = 4
;

output [
"x: " ++ show(x) ++ "\n"++
"n: " ++ show(n) ++ "\n"
];
