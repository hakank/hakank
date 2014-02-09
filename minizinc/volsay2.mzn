% 
% Volsay problem in MiniZinc.
% 
% From OPL model volsay.mod.
% Now with array
%
% Compare with the Comet model:
% http://www.hakank.org/comet/volsay2.co

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: gas = 1;
int: chloride = 2;
set of int: Products = 1..2;

% decision variables
array[Products] of var 0.0..100.0: production;

var 0.0..10000.0: z = 40.0 * production[gas] + 50.0 * production[chloride] ;

% solve maximize z;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
solve :: float_search(production, 0.00001, input_order, indomain_split, complete) maximize z;

constraint
  production[gas] + production[chloride] <= 50.0 /\
  3.0 * production[gas] + 4.0 * production[chloride] <= 180.0 /\
  production[chloride] <= 40.0

;

output [
  "production: " ++ show(production) ++ "\n" ++ 
  "z: " ++ show(z) ++ "\n"
];
