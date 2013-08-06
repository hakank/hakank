% 
% Public School Problem in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 6
% Description  : Public School Problem
% Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s6.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

array[1..5] of var int: x;
var 21..29: m;

solve minimize x[1];

constraint
  forall(i in 1..5) (
     x[i] >= m
     /\
     x[i] <= 1000
  )
  /\
  % the float version don't work
  % x[1] = 0.5 * x[5] - 2.0
  % /\
  % x[4]+x[5] = x[2] + x[3] + 14.0
  % /\
  % x[2]+x[5] = sum(i in 1..5) (0.5 * x[i]) - 2.0
  % /\
  % x[3]+x[4] = sum(i in 1..5) ((13.0/32.0) * x[i])

  % integer version
  2*x[1] = x[5] - 4  % multiply with 2
  /\
  x[4]+x[5] = x[2] + x[3] + 14
  /\
  2*x[2]+2*x[5] = sum(i in 1..5) (x[i]) - 4 % multiply with 2
  /\
  32*x[3]+32*x[4] = 13*sum(i in 1..5) (x[i]) % factor out the division
;

output
[
 show(x)
];