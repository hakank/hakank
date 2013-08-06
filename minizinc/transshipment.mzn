%
% Transshipment problem in Minizinc
% 
% Winston "Operations Research", page 400ff 
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of 1..6: cities = 1..6;
array[cities, cities] of int: cost;
array[cities] of int: supply;
array[cities] of int: demand;

% decicion variables
array[cities, cities] of var 0..10000: delivery; % >= 0;
var 0..10000: z = sum(i in cities, j in cities) (cost[i,j]*delivery[i,j]);

% eclipse eplex is fast
% solve minimize z;
solve :: int_search(
        [delivery[i,j] | i, j in cities] % ++
        % ++ [z]
        , 
        largest, 
        indomain_max, 
        complete) 
    minimize z;


constraint
 % forall(i, j in cities) (
 %   delivery[i,j] >= 0 
 %   /\ delivery[i,j] <= 10000 
 % )
 % /\
 forall(i in cities) (
        sum(j in cities) (delivery[i,j]) <= supply[i]
 )
 /\
 forall(j in cities) (
     sum(i in cities) (delivery[i,j]) >= demand[j]
 )
;


cost = 
array2d(cities, cities,
[
   0, 1000,     8,    13,    25,    28,
1000,    0,    15,    12,    26,    25,
1000, 1000,     0,     6,    16,    17,
1000, 1000,  1000,     0,    14,    16,
1000, 1000,  1000,  1000,     0,  1000,
1000, 1000,  1000,  1000,  1000,     0,
])
;

supply = [150, 200, 9999, 9999, 0, 0]; % ,9999];
demand = [0, 0,9999,9999, 130, 130]; %,9999];

output [
  "z: ", show(z)
] ++
[
  if j = 1 then "\n" else " " endif ++
  show(delivery[i,j])
  | i,j in cities

];
