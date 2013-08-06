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

int: m = 4; % Number of variables
set of int: M = 1..m;

array[M] of var 0..711: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  711 = sum(i in M) (x[i])
  /\
  % 711000000 = x[1]*x[2]*x[3]*x[4]
  711*100*100*100 = x[1]*x[2]*x[3]*x[4]

  /\ % symmetry breaking
  forall(i in 2..m) (
    x[i-1] <= x[i]
  )
;  

output [
 "x: ", show(x), "\n"

];
