% 
% Scalar product in MiniZinc.
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 10;
array[1..n] of int: weights = [1,2,3,10,4,2,5,3,8,5];

% decision variables
array[1..n] of var 0..1: x;

var 0..sum(weights): z;


predicate scalar_product(array[int] of var int: x, array[int] of var int: v, var int: s) =
  s = sum(i in index_set(x)) (x[i]*v[i])
;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  scalar_product(x,weights,z)
;

output [
   "weights: " ++ show(weights) ++ "\n" ++
   "z: " ++ show(z) ++ "\n" ++
   "x: " ++ show(x) ++ "\n"
];

