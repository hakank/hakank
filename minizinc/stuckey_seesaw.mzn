%
% Simple modelling in Minizinc.
% 
% Marriott & Stuckey "Programming with Constraints", page 257.
% 
% Balancing on a seesaw.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

var -5..5: Liz;
var -5..5: Fi;
var -5..5: Sara;

predicate apart(var int: x, var int: y, int: n) =
          x >= y + n \/ y >= x + n
;

solve satisfy;

constraint
        9 * Liz + 8 * Fi + 4 * Sara = 0
        /\
        apart(Liz, Fi, 3)   /\ 
        apart(Liz, Sara, 3) /\
        apart(Sara, Fi, 3)
        /\ % symmetry breaking
        Sara >= 0
        % /\ % page 258 suggests cumulative instead of apart.
        % cumulative([Liz, Fi, Sara], [3,3,3], [1,1,1], 1)
;        

output [
   "Liz: ", show(Liz), "\n",
   "Fi: ", show(Fi), "\n",
   "Sara: ", show(Sara), "\n"
];



