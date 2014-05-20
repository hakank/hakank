% 
% Numerica problem page 24 in MiniZinc.
% 
% Problem from neuropsysiology.
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 4;
int: m = 6;

array[1..n] of float: C = [5.1,4.3,3.1,2.3];

% decision variables
array[1..m] of var float: x;

% solve satisfy;
solve :: float_search(x, 0.001, input_order, indomain_split, complete) satisfy;


constraint
  x[1]*x[1] + x[3]*x[3]  = 1.0 /\
  x[2]*x[2] + x[4]*x[4]  = 1.0 /\
  x[5]*x[3]*x[3]*x[3] + x[6]*x[4]*x[4]*x[4] = C[1] /\
  x[5]*x[1]*x[1]*x[1] + x[6]*x[2]*x[2]*x[2] = C[2] /\
  x[5]*x[1]*x[3]*x[3] + x[6]*x[4]*x[4]*x[2] = C[3] /\
  x[5]*x[1]*x[1]*x[3] + x[6]*x[2]*x[2]*x[4] = C[4] 

;

output [
  "x:" ++ show(x) ++ "\n"
];

