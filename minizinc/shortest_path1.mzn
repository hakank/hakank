%
% Shortest path problem in Minizinc.
% 
% Problem and model from Winston "Operations Research", page 415.
%
% When to buy/sell a car depending on the cost of maintenance etc.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: timex; % >= 0;
int: new_car; % >= 0;
array[1..timex] of int: trade_in;
array[1..timex] of int: maintenance;
array[1..timex+1] of int: supply;
array[1..timex+1] of int: demand;

% cost matrix
array[1..timex, 1..timex+1] of var int: cost;

% the "delivery-table" (in transshipment parlor)
array[1..timex, 1..timex+1] of var int: x;

% objective to minimize: total cost
var int: z = sum(i in 1..timex, j in 1..timex+1) (x[i,j]*cost[i,j]); 

% minimize the cost
% solve :: int_search([x[i,j] | i in 1..timex, j in 1..timex+1] , "first_fail", "indomain", "complete")  minimize z;
solve minimize z;

constraint
  forall(i in 1..timex,j in 1..timex+1) (
    cost[i,j] >= 0 /\
    x[i,j] >= 0
  )
  /\

  % cost matrix:
  % cost[i,j] =   maintenance cost incurred during years i, i+1,..j-1 
  %             + cost of purchasing new car at beginning of year i 
  %             - trade-in value received at beginning of year j
  forall(i in 1..timex, j in i+1..timex+1) (
    cost[i,j] = new_car + ( sum(k in 1..j-i) (maintenance[k])) - trade_in[j-i]
  )
  /\

  % new_car^2 in the lower diagonal
  forall(i in 1..timex, j in 1..timex+1 where j < i) (
       cost[i,j] = new_car*new_car
  )
  /\
  % rows represent the timex
  forall(i in 1..timex) (
        sum(j in 1..timex+1) (x[i,j]) = supply[i]
   )
  /\
  % what strategy to choose
  forall(j in 1..timex+1) (
        sum(i in 1..timex) (x[i,j])  = demand[j]
  )
;

timex = 5;
new_car = 12;

% car trade-in price, in 1000 dollar
trade_in = [7, 6, 2, 1, 0];

% cost to repair a car year x
% in 1000 dollar
maintenance = [2,  4, 5, 9,12];


% The "transshipment table" which is the key to this
% type of modelling.
% 2..5 is transshipment points with "unlimited" supply and demand
% 
supply = [1,11,11,11,11,0];
demand = [0,11,11,11,11,1];

output [
   if i = 1 /\ j = 1 then 
    "\nz: " ++ show(z) ++     
    "\nx:" else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in 1..timex, j in 1..timex+1
] ++ [
   if i = 1 /\ j = 1 then 
     "\n\nCost:" else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(cost[i,j])
   | i in 1..timex, j in 1..timex+1
];
