% 
% Seven11 puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 12
% Source: alt.math.recreational
%
% This model was inspired by the AMPL model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s12.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";
array[1..4] of var 0..711: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  711 = sum(x) 
  /\
  711 * 100 * 100 * 100 = product(x) 
  /\ 
  % symmetry breaking
  increasing(x)
;  

output [
 "x: ", show(x), "\n"
];
