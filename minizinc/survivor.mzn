% 
% Survivor problem in MiniZinc.
% 
% From PuzzlOR
% Survivor
% http://www.analytics-magazine.org/july-august-2010/137-puzzlor-survivor.html
% """
% By John Toczek
%  
% Getting lost while hiking in the wilderness is a dangerous situation to 
% find yourself in. And making your way back to civilization is a difficult 
% task that quickly uses up resources. What you decide to take with you while 
% making the journey back to civilization can determine life or death.
% 
% Table 1 shows all of the items that are available to you that will aid you in 
% your hike out of the wilderness. Containers of food and water will give you 
% energy, shelter will protect you from the elements, and defense will protect 
% you from wild animals. Each item has a weight indicated by the red number and 
% each item has survival points indicated by the green number. You must take one 
% item from each of the four categories (food, water, shelter, defense). 
% Unfortunately, the backpack you have has a maximum capacity of 25 kg. Your 
% chance for survival is calculated by adding all of the survival points together 
% from the items you choose to take with you.
% 
% Table 1: Choose items carefully.
% [
%         Weight (kg)  Survival (pts)
%    Food    5          10
%            8          20
%           12          25
% 
%    Water   3          10
%            5          20
%            8          25
% 
%    Shelter 5           5
%            8          15
%           12          20
% 
%    Defence 1           5
%            2          15
%            3          20 
% ]
% 
% Question: What is the maximum chance for survival you can achieve?
%
% """

% This is the unique solution:
% 
%   sum_weight: 24
%   sum_survival_points: 75
%   x: [2, 2, 2, 3]
%   food   : 8kg 20pts 
%   water  : 5kg 20pts 
%   shelter: 8kg 15pts 
%   defence: 3kg 20pts 
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: num_types = 4; % number of types
int: num_items = 3; % items per type

% the items: type, weight, survival pts
array[1..num_items, 1..2] of int: food = 
  array2d(1..num_items, 1..2,
 [  
  % Weight (kg)  Survival (pts)
    5,          10,
    8,          20,
   12,          25,
 ]);

array[1..num_items, 1..2] of int: water = 
  array2d(1..num_items, 1..2,
 [  
    3,          10,
    5,          20,
    8,          25
]);

array[1..num_items, 1..2] of int: shelter = 
  array2d(1..num_items, 1..2,
 [  
    5,           5,
    8,           15,
   12,           20
 ]);

% Defence
array[1..num_items, 1..2] of int: defence = 
  array2d(1..num_items, 1..2,
 [  
    1,           5,
    2,          15,
    3,          20
 ]);

int: max_cap = 25; % max capacity

% decision variables 
array[1..num_types] of var 1..num_items: x;
var int: sum_weight;
var int: sum_survival_points;

% solve satisfy;
solve maximize sum_survival_points;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  sum_weight = food[x[1],1]+water[x[2],1]+shelter[x[3],1]+defence[x[4],1]
  /\
  sum_weight <= max_cap
  /\
  sum_survival_points = food[x[1],2]+water[x[2],2]+shelter[x[3],2]+defence[x[4],2]
  
;

% for solve satisfy
% constraint sum_survival_points = 75;

output [
  "sum_weight: " ++ show(sum_weight) ++ "\n" ++
  "sum_survival_points: " ++ show(sum_survival_points) ++ "\n" ++
  "x: " ++ show(x)
]
++
[
  "\nfood   : " ++ show(food[fix(x[1]),1]) ++ "kg " ++ show(food[fix(x[1]),2]) ++ "pts " ++ "\n" ++
  "water  : " ++ show(water[fix(x[2]),1]) ++ "kg " ++ show(water[fix(x[2]),2]) ++ "pts " ++ "\n" ++
  "shelter: " ++ show(shelter[fix(x[3]),1]) ++ "kg " ++ show(shelter[fix(x[3]),2]) ++ "pts " ++ "\n" ++
  "defence: " ++ show(defence[fix(x[4]),1]) ++ "kg " ++ show(defence[fix(x[4]),2]) ++ "pts " ++ "\n"

]
++ ["\n"]
;

