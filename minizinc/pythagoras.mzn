% 
% Pythagoras in MiniZinc. 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

var int: A;
var int: B;
var int: C;

solve satisfy;

constraint
    A >= 1 /\ B >= 1 /\ C >= 1
    /\
    A*A + B*B =C*C 
    /\
    A < B /\ B < C
;


output [
  show(A),"^2 + ", show(B), "^2 = ", show(C), "^2", "\n"
  
]