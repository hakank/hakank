% 
% Satisfication problem in MiniZinc.
% 
% From the Alma-0 model satisfy.a0
% (http://www.cwi.nl/en/alma)
% """
% The problem is to generate all interpretations that satisfy a given 
% propositional formula.
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: N = 4;

% array[1..N] of var bool: x;
array[1..N] of var 0..1: x;

solve satisfy;

constraint

 ((x[1] = 1 /\ x[2] = 1) \/ (x[3] = 1 /\ x[4] = 1))
 /\
 ((x[1] != x[3]) \/ (x[2] != x[4]))

;

output [show(x)];