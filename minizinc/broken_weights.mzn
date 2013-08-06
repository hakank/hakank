%
% Broken weights problem in Minzinc
% 
% From
% http://www.mathlesstraveled.com/?p=701
% """
% Here's a fantastic problem I recently heard. Apparently it was first 
% posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
% published in 1612, and can also be found in Heinrich Dorrie’s 100 
% Great Problems of Elementary Mathematics.
% 
%     A merchant had a forty pound measuring weight that broke 
%     into four pieces as the result of a fall. When the pieces were 
%     subsequently weighed, it was found that the weight of each piece 
%     was a whole number of pounds and that the four pieces could be 
%     used to weigh every integral weight between 1 and 40 pounds. What 
%     were the weights of the pieces?
% 
% Note that since this was a 17th-century merchant, he of course used a 
% balance scale to weigh things. So, for example, he could use a 1-pound 
% weight and a 4-pound weight to weigh a 3-pound object, by placing the 
% 3-pound object and 1-pound weight on one side of the scale, and 
% the 4-pound weight on the other side.
% """
%
% Also, compare with the coin change problem:
%   http://www.hakank.org/minizinc/coins3.mzn
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn";

int: n = 4; % the number of different weights
int: m = 40; % original weight
array[1..n] of var 1..m: weights; % the weights
array[1..m, 1..n] of var -1..1: x; % the combinations

% solve satisfy;
% solve :: int_search(weights, input_order, indomain_min, complete) satisfy;
solve :: int_search(weights, occurrence, indomain_min, complete) minimize weights[n];


constraint

        % symmetry breaking
        forall(i in 2..n) (
          weights[i-1] <= weights[i] 
        )
        /\
        sum(weights) = m

        /\ % Check that all weights from 1 to 40 can be made.
           %  
           % Since all weights can be on either side
           % of the side of the scale we allow either
           % -1, 0, or 1 or the weights, assuming that
           % -1 is the weights on the left and 1 is on the right.
           % 
        forall(j in 1..m) (
          sum(i in 1..n) (x[j,i]*weights[i]) =j 
        )

        %% version where no x array is used
        % forall(j in 1..m) (
        % let {
        %    array[1..n] of var -1..1: tmp} 
        % in
        % sum(i in 1..n) (tmp[i]*weights[i]) = j
        % )

;

%
% Somewhat nicer output (for the minizinc solver).
%
output [
  "W:    "
] ++ 
[
   show(weights[i]) ++ "   " 
   | i in 1..n
] ++ ["\n"] ++
[
  if i = 1 then "\n" ++ show_int(2,j) ++ ": " else " " endif ++
     show_int(3, x[j,i])  
  | j in 1..m, i in 1..n
] ++ 
["\n"];