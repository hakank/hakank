% 
% Number of fix points in MiniZinc.
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 7;

array[1..n] of var 1..n: x;
var int: z; % number of fixpoints in x

solve satisfy;

%
% fix_points
%
predicate fix_points(var int: a, array[int] of var int: x) =
    a = sum(i in 1..length(x)) (bool2int(x[i] = i))
;

constraint
  fix_points(z, x)
  /\
  z = 4

;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];
