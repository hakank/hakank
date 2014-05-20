% 
% Golden search in MiniZinc.
% 
% From Winston Operations Research, page 654 (problem 2). 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

var -1.0..3.0: x;
var float: z;
% solve maximize x - exp(x);
solve :: float_search([x], 0.001, input_order, indomain_split, complete) maximize z;


constraint 
 z  = x - exp(x)
 % -1.0 <= x /\ x <= 3.0
;

output 
[
   "x: ", show(x), "\n",
   "z: ", show(z), "\n",
]