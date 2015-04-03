% 
% Sultan's children in MiniZinc.
% 
% From "Puzzle-based learning", page 80
% """
% Many years ago, a powerful sultan had the largest harem int the 
% world. He had many children with his many wifes, and toward the 
% end of his life the total number of his children was estimated
% to be between 100 and 500. However, the sultan kept the exact number
% of his children a secret: no one could provide a better estimation of
% this number.
%
% One day a foreign diplomat overheard a conversation between the sultan
% and his vizier. The sultan said: "If you select any two of my children
% at random, the probability that you selected two boys would be exactly
% 50 percent.
% 
% This piece of information was sufficient for the diplomat to calculate
% the exact number of the sultan's children. How many children did
% the sultan have?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

var 1..500: b; % boys
var 1..500: g; % girls



solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   %% The equation at page 81
   %% Not all FlatZinc solvers can handle this 
   % (b*(b-1)) / ( (b+g)*(b+g-1) )  = 0.5

   %% Variant
   % 2*(b*(b-1)) / ( (b+g)*(b+g-1) )  = 1

   2*(b*(b-1)) = ( (b+g)*(b+g-1) ) 

   /\ b + g >= 100 
   /\ b + g <= 500 
;

output [
  "b: ", show(b), "\n",
  "g: ", show(g), "\n",
  "total: ", show(b+g), "\n"
];

