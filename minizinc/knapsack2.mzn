% 
% Another knapsack problem in Minizinc.
% 
% This example is from a LINGO model
% http://www.icaen.uiowa.edu/~ie166/Private/Lingo.pdf
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

int: n; % number of objects
int: capacity;  % capacity (max total weight) of the knapsack
array[1..n] of int: weights; % weight of each product
array[1..n] of var int: take; % decision variable: 1 if we take item i; 0 otherwise
array[1..n] of int: ratings; % the ratings of each product

% optimize ratings
var int: profit = sum(i in 1..n) (take[i] * ratings[i]);

% simple knapsack
predicate knapsack(array[int] of var int: Weights, array[int] of var int: Take, var int: Wtmax) =
           sum(i in index_set(Weights)) ( Weights[i] * Take[i]) <= Wtmax
;

solve maximize profit;
% solve satisfy;

constraint
   % all elements in take must be >= 0
   forall(i in 1..n) (  
         take[i] >= 0  
         /\ take[i] <= 1 % max one product
   )
   /\
   % and then use the knapsack predicate
   knapsack(ratings, take, capacity)
%   /\ profit >= 15 % for solve satisfy
;


%
% data
%
n = 8;
capacity = 15;


% ant_repel, beer, blanket, bratwurst, brownies, frisbee, salad, watermelon
weights = [1,3,4,3,3,1,5,10]; 
ratings = [2,9,3,8,10,6,4,10];

output [
   "profit: ", show(profit), "\n", 
   show(take), "\n"
];




