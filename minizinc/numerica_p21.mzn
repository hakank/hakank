% 
% Numerica problem page 19 in MiniZinc.
% 
% Problem from the Numerica book, page 21.
% Array variant of numerica_p20.mzn
% 
% JaCoP: two solutions
% x:[-0.78615137775742, 0.61803398874989]
% ----------
% x:[0.78615137775742, 0.61803398874989]

% 
% Gecode give these two solutions twice.
%


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

float: v = pow(10.0,8.0);

array[1..2] of var -v..v: x;


% solve satisfy;
solve :: float_search(x, 0.00000000000001, input_order, indomain_split, complete) satisfy;

constraint
   x[1]*x[1] + x[2]*x[2] = 1.0 
   /\
   x[1]*x[1] = x[2]
   
   % additional constraint
   % /\ x[1] >= 0.0
;

output [
  "x:" ++ show(x) ++ "\n"
];

