% 
% Farm puzzle in MiniZinc.
% 
% From http://www.cs.st-andrews.ac.uk/~andrea/tailor/examples.html
% """
% A farmer has 7 animals on his farm: pigs and hens.
% They all together have 22 legs. How many pigs (4 legs)
% and how many hens (2 legs) does the farmer have?
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

var int: number_of_legs = 22;
set of int: feet = {2,4}; % number of feet
array[1..7] of var feet: x; % the seven animals
array[0..4] of var int: c; % for global_constraints which start to count at 0

solve satisfy;

constraint
  sum(x) = number_of_legs
  /\
  increasing(x) % symmetry breaking (to make the solution unique)
  /\
  % global_cardinality_old(x,c)
  global_cardinality(x,array1d(0..4, set2array(index_set(c))), c)
;

output [
  "number of hens (2 legs): ", show(c[2]), "\n",
  "number of pigs (4 legs): ", show(c[4]), "\n"

];