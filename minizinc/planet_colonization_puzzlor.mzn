% 
% Planet Colonization (PuzzlOR) in MiniZinc.
% 
% http://puzzlor.com/2010-02_PlanetColonization.html
% [Note: the link don't work in all solvers, if any.]
% """
% Planet Colonization
% 
% By John Toczek
% Colonizing a new planet where conditions are unpredictable and harsh 
% is never easy. The primary concern when selecting a landing site for 
% the first colony is the proximity to natural resources. Access to these 
% valuable resources determines whether or not a new colony thrives or dies.
% 
% Figure 1 shows the front and rear views of a newly discovered planet. 
% The planet has been divided into 20 areas, and each area is a potential 
% site to establish a new colony. Some areas contain value resources 
% (represented by colored icons) that are needed in order for the new colony 
% to survive. 
%
% Food is represented by the green ear of corn, 
% oxygen by the red O2, 
% water by the blue drop and 
% energy by the orange lightning bolt.
% 
% When choosing a landing site, it is best to minimize the distance between 
% that site and the four needed resources. Distance is calculated by the 
% number of units it takes to get to the resource. For example, if area 
% 9 was selected as the landing site, the total travel distance required 
% to reach all four resources would be six units (two for energy, one for good, 
% one for water and two for oxygen).
%
% Question:
% Which of the 20 areas is the best landing site to minimize the total distance 
% you should have to travel to all four resources
% """
%
% Also, see http://www.orms-today.org/orms-2-10/puzzlor.html
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; % number of slots

% where are the resources placed?
int: food = 1;
int: oxygen = 2;
int: water = 3;
int: energy = 4;
array[1..n] of int: resources =
[
  0,      % 1
  energy, % 2 
  0,      % 3
  0,      % 4
  0,      % 5
  0,      % 6
  food,   % 7
  water,  % 8
  0,      % 9
  0,      % 10
  0,      % 11
  energy, % 12
  0,      % 13
  oxygen, % 14
  0,      % 15
  water,  % 16
  0,      % 17
  0,      % 18
  oxygen, % 19
  food,   % 20
 ];

set of 1..n: energy_sites = {2,12};
set of 1..n: food_sites   = {7,20};
set of 1..n: water_sites  = {8,16};
set of 1..n: oxygen_sites = {14,19};

array[1..n,1..n] of int: distance;

int: num_connections;
array[1..num_connections, 1..2] of int: connections;


% decision variables
var 1..n: landing_site;
var 0..20*20: cost; 

var energy_sites: energy_selected;
var food_sites: food_selected;
var water_sites: water_selected;
var oxygen_sites: oxygen_selected;

solve minimize cost;
% solve satisfy;

constraint
  % landing_site = 9 /\ % test of the example mentioned in the problem statement

  cost = distance[landing_site, energy_selected] +
         distance[landing_site, food_selected] +
         distance[landing_site, water_selected] +
         distance[landing_site, oxygen_selected]

  % /\ % if there is an requirement that we cannot land on a resource site
  % landing_site in {i | i in 1..n where resources[i] = 0}

  % /\ cost = 5 % testing uniqueness of the solution
;

output 
[
  "landing_site   : ", show(landing_site), "\n",
  "cost           : ", show(cost), "\n",
  "energy_selected: ", show(energy_selected), " cost: ", show(distance[fix(landing_site),fix(energy_selected)]), "\n",
  "food_selected  : ", show(food_selected), " cost: ", show(distance[fix(landing_site),fix(food_selected)]),  "\n",
  "water_selected : ", show(water_selected), " cost: ", show(distance[fix(landing_site),fix(water_selected)]),  "\n",
  "oxygen_selected: ", show(oxygen_selected), " cost: ", show(distance[fix(landing_site),fix(oxygen_selected)]),  "\n",
];


%
% Data
%
n = 20;

%
% This is not used directly.
%
num_connections = 60;
connections = 
array2d(1..num_connections, 1..2,
[
 1,2,
 1,8,
 1,5,
 2,1,
 2,10,
 2,3,
 3,4,
 3,12,
 3,2,
 4,3,
 4,14,
 4,5,
 5,4,
 5,1,
 5,6,
 6,7,
 6,5,
 6,15,
 7,6,
 7,8,
 7,17,
 8,7,
 8,1,
 8,9,
 9,8,
 9,10,
 9,20,
 10,9,
 10,2,
 10,11,
 11,10,
 11,19,
 11,12,
 12,11,
 12,3,
 12,13,
 13,12,
 13,14,
 13,18,
 14,13,
 14,4,
 14,15,
 15,14,
 15,16,
 15,6,
 16,15,
 16,18,
 16,17,
 17,16,
 17,20,
 17,7,
 18,16,
 18,19,
 18,13,
 19,18,
 19,11,
 19,20,
 20,19,
 20,17,
 20,9,
]);

% This all-pair distance is from another program 
% (using Floyd-Warshall algorithm).
%
distance = 
array2d(1..n,1..n,
[
% 1                10                   
  0,1,2,2,1,2,2,1,2,2,3,3,4,3,3,4,3,5,4,3,
  1,0,1,2,2,3,3,2,2,1,2,2,3,3,4,5,4,4,3,3,
  2,1,0,1,2,3,4,3,3,2,2,1,2,2,3,4,5,3,3,4,
  2,2,1,0,1,2,3,3,4,3,3,2,2,1,2,3,4,3,4,5,
  1,2,2,1,0,1,2,2,3,3,4,3,3,2,2,3,3,4,5,4,
  2,3,3,2,1,0,1,2,3,4,5,4,3,2,1,2,2,3,4,3,
  2,3,4,3,2,1,0,1,2,3,4,5,4,3,2,2,1,3,3,2,
  1,2,3,3,2,2,1,0,1,2,3,4,5,4,3,3,2,4,3,2,
  2,2,3,4,3,3,2,1,0,1,2,3,4,5,4,3,2,3,2,1,
  2,1,2,3,3,4,3,2,1,0,1,2,3,4,5,4,3,3,2,2,
  3,2,2,3,4,5,4,3,2,1,0,1,2,3,4,3,3,2,1,2,
  3,2,1,2,3,4,5,4,3,2,1,0,1,2,3,3,4,2,2,3,
  4,3,2,2,3,3,4,5,4,3,2,1,0,1,2,2,3,1,2,3,
  3,3,2,1,2,2,3,4,5,4,3,2,1,0,1,2,3,2,3,4,
  3,4,3,2,2,1,2,3,4,5,4,3,2,1,0,1,2,2,3,3,
  4,5,4,3,3,2,2,3,3,4,3,3,2,2,1,0,1,1,2,2,
  3,4,5,4,3,2,1,2,2,3,3,4,3,3,2,1,0,2,2,1,
  5,4,3,3,4,3,3,4,3,3,2,2,1,2,2,1,2,0,1,2,
  4,3,3,4,5,4,3,3,2,2,1,2,2,3,3,2,2,1,0,1,
  3,3,4,5,4,3,2,2,1,2,2,3,3,4,3,2,1,2,1,0,
]);
