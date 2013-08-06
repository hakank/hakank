%
% General version of transportation problem in Minizinc.
% Cf. transportation.mzn 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_plants;
int: num_clients;

array[1..num_plants] of int: capacity;
array[1..num_clients] of int: demand;
array[1..num_plants, 1..num_clients] of int: cost;
array[1..num_plants, 1..num_clients] of var int: x;

var int: z = sum(i in 1..num_plants, j in 1..num_clients) (x[i,j]*cost[i,j]);

% solve minimize z;
% solve satisfy;
solve :: 
  int_search([x[i,j] | i in 1..num_plants, j in 1..num_clients], 
        first_fail, indomain_min, complete) 
  minimize z;

constraint
        forall(i in 1..num_plants, j in 1..num_clients) (
           x[i,j] >= 0 
        )
        /\ 
        forall(i in 1..num_plants) (
           sum(j in 1..num_clients) (x[i,j]) <= capacity[i]
        )
        /\ 
        forall(j in 1..num_clients) (
           sum(i in 1..num_plants) (x[i,j]) = demand[j]
        )

%         /\ z <= 6600 % for solve satisfy
;

%
% data
%

num_plants = 3;
num_clients = 4;
capacity = [500, 300, 400];
demand = [200, 400, 300, 100];

cost = [|10, 8, 5, 9,
        |7, 5, 5, 3,
        |11,10, 8, 7|];



output [
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z)
];