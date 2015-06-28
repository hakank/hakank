% 
% Funny dice (Sunday Times teaser 2739) in MiniZinc.
% 
% http://ccgi.gladman.plus.com/wp/?page_id=2671
% """
% Sunday Times Teaser 2739 – Funny Dice
% by Michael Fletcher
%
% I have two non-standard cubic dice, one red and one blue, each with positive whole numbers 
% on each face. The total of the six numbers on the red die is higher than the same total for the blue die.
% 
% When I repeatedly throw them and add their two values the results, in least to most likely order, 
% are: 2/12, 3/11, 4/10, 5/9, 6/8 and 7.
% 
% What are the numbers on the red dice?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;

% decision variables
% Note: It doesn't matter if we use "var int" or var 1..12 as the domains.
% Gecode solves this in 48ms using var int, and 38ms using var 1..12.
% In "production", I always use as small domains as possible (but not smaller).
array[1..n] of var 1..12: red;
array[1..n] of var 1..12: blue;
array[2..12] of var 1..12: probs;

% array[1..n] of var int: red;
% array[1..n] of var int: blue;
% array[2..12] of var int: probs;

% solve satisfy;
solve :: int_search(probs ++ red ++ blue, occurrence, indomain_min, complete) satisfy;

constraint
 % domains
 forall(i in 1..n) ( red[i] >= 1 /\ blue[i] >= 1) /\

 % The total of the six numbers on the red die is higher than the same total for the blue die.
 sum(red) > sum(blue) /\

 % ensure the probabilities
 forall(k in 2..12) (
   probs[k] >= 1 /\ % domains
   probs[k] = sum(i,j in 1..n) ( red[i]+blue[j] == k)
 )
 /\ % the relations of the probabilities
 forall(i in 2..6) (
   probs[i] = probs[12-i+2] /\
   probs[i] < probs[i+1] /\
   probs[6+i-1] > probs[6+i] 
 )
 /\ probs[7] = max(probs)

 % symmetry breaking (without it, it takes longer time)
 /\ increasing(red) 
 /\ increasing(blue) 

;

output [
  "probs: ", show(probs), "\n",
  "red: ", show(red), "\n",
  "blue: ", show(blue), "\n",
];

