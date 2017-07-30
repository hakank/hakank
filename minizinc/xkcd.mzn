%
% xkcd Knapsack problem in Minizinc.
%
%  http://xkcd.com/287/
%
% Some amount (or none) of each dish should be ordered to give a total of exact 15.05
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_prices;
array[1..num_prices] of int: price;
int: total;
% array[1..num_prices] of var int: x; % how many items of each dish
array[1..num_prices] of var 0..100000: x; % how many items of each dish

var int: z = sum(i in 1..num_prices) (x[i]);

% solve minimize z;
solve satisfy;

constraint
  total = sum(i in 1..num_prices) (x[i]*price[i])
  % /\ forall(i in 1..num_prices) (x[i] >= 0) % when x don't have an explicit domain.
  % /\ z >= 0
;

%
% data
%
num_prices = 6;
price = [215, 275, 335, 355, 420, 580];
total = 1505; % multiply by 100 to be able to use integers


output [
  show(x), "\n"
];
