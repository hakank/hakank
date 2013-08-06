% 
% Tommy's Birthday Coins coins puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 2.
% Description  : Tommy's Birthday Coins
% Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s2.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: coin = 3;
set of int: C = 1..coin;
array[C] of var int: x;
array[C] of int: value = [30,12,6];

solve satisfy;

constraint
   forall(i in C) (x[i] >= 1) 
   /\
   sum(i in C) (value[i]*x[i]) = 306
   /\ 
   sum(i in C) (x[i]) = 15
;


output [
  "x: ", show(x)
];
