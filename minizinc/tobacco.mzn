% 
% Tobacco puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 1.
% Description  : Clarke's tobacconist
% Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s1.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

array[1..2] of var int: x;

solve satisfy;

constraint 
  25*x[1] + 49*x[2] = 2000
  /\
  forall(i in 1..2) (
    x[i] >= 0
  )
;


output [show(x)];