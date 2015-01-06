% 
% Divisors ending in 0 to 9 puzzle in MiniZinc.
% 
% From Card Colm Mulcahy
% https://twitter.com/CardColm/status/547518933398732803
% """
% RT of @WWMGT (from MG book): Find the smallest integer which has 
% divisors ending in 0,1,2,3,4,5,6,7,8 and 9 (problem via @BruceReznick )
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

var 1..1000: n;

% solve satisfy;
solve :: int_search([n], first_fail, indomain_split, complete) satisfy;

constraint
  forall(i in 0..9) (
    sum([bool2int(n mod d = 0 /\ d mod 10 = i) | d in 1..ub(n) div 2]) > 0
  )

;

output [
  show(n)
];

