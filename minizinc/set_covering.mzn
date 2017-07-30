%
% Simple set covering problem in Minizinc.
% 
% Placing of firestations, from Winston "Operations Research", page 486
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% where to place the fire stations
array[1..numCities] of var 0..1: x; 

% distance between the cities
array[1..numCities, 1..numCities] of int: distance; 

int: min_distance;
int: numCities;

% objective: minimize the number of fire stations
var int: z= sum( [x[i] | i in 1..numCities] );

solve minimize z; 
% solve satisfy;

constraint 
   forall(j in 1..numCities) 
   (
       sum([x[i] | i in 1..numCities where distance[i,j] <= min_distance] ) >= 1
   )


;

%
% data
%
min_distance = 15;
numCities = 6;

distance = 
array2d(1..numCities, 1..numCities,
[0,10,20,30,30,20,
10, 0,25,35,20,10,
20,25, 0,15,30,20,
30,35,15, 0,15,25,
30,20,30,15, 0,14,
20,10,20,25,14,0])
;


output
[
  "x: " ++ show(x) ++ "\nz: " ++ show(z)
];