% 
% Simple knapsack problem in Minizinc.
%  
% ( Cf http://www.hakank.org/constraints/Knapsack.java )

%
% This Minizinc program is written by Hakan Kjellerstrand and is commented in 
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%

%
% Problem from http://sourceforge.net/forum/forum.php?thread_id=1432186&forum_id=335511
%
% Knapsack maximization problem example
% @author Fernando Lopez Hernandez (f.lopezATuamDOTes)
%
% In this problem a thief have a knapsack with capacity of 10 units.
% He could charge the knapsack with golden ingots of size 4, silver ingots
% of size 3, and bronze ingots of size 2. Each ingot value is 15, 12 and 7
% respectively.
%
% The solver goal is to find a solution who maximize profit with the above
% restrictions. That is to say: If G represents the number of golden ingots,
% S the number of silver ingots, B the number of bronze ingots,
% and P the profit, we define the following constraints:
% 4G + 3S + 2B <= 10
% 15G + 12S + 7B = P

% Solution:
% [ 1, 2, 0 ]
% i.e. 1 Gold, 2 Silver and 0 Bronze
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Define a predicate for generality
predicate knapsack(array[int] of var int: Weights, array[int] of var int: Take, var int: Wtmax) =
           sum(i in index_set(Weights)) ( Weights[i] * Take[i]) <= Wtmax
;


int: n; % number of objects
int: weight_max;  % maximum weight allowed (the capacity of the knapsack)
array[1..n] of int: values;
array[1..n] of int: weights;
array[1..n] of var int: take; % 1 if we take item i; 0 otherwise

var int: profit = sum(i in 1..n) (take[i] * values[i]);

solve maximize profit;

constraint
   % all elements in take must be >= 0
   forall(i in 1..n) (  take[i] >= 0  )
   /\
   % and then use the knapsack predicate
   knapsack(weights, take, weight_max)
;


% data
n = 3;
weight_max = 10;

% Gold, Silver, Bronze        
values = [15, 12, 7];
weights = [4, 3, 2]; 

output [
   show(take), "\n"

];
