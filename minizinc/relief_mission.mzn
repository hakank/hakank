% 
% Relief Mission in MiniZinc.
% 
% From PuzzlOR
% Relief Mission
% http://www.analytics-magazine.org/september-october-2010/122-the-puzzlor-relief-mission.html
% """
% Coordinating relief efforts after catastrophes such as civil unrest and 
% natural disasters can be a logistically complex challenge. Delivering 
% relief to people in need is the immediate focus of any disaster management plan.
% 
% The map in Figure 1 shows the locations of 20 villagers, each represented by 
% a "hut" icon. The villagers are in need of relief supplies contained in 
% the crates attached to parachutes. There are two identical relief packages 
% available. The only delivery option is by air drop. Each package can be dropped 
% on any cell.
% 
% After the crates are dropped, each villager will walk to the nearest drop 
% location to pick up relief supplies. Use a direct line between cells to calculate 
% travel distance. For example, the distance between A1 and A2 is 1km and the 
% distance between A1 to B2 is 1.41 km. Assume that each crate contains an 
% unlimited amount of relief supplies.
% 
% Figure 1: Where should the two relief packages be dropped?
% [
%   1 2 3 4 5 6 7 8 9 10
%   0,0,0,0,1,0,0,0,0,0, % A
%   0,0,0,0,1,0,0,0,1,1, % B
%   1,0,0,0,0,1,0,1,1,1, % C
%   0,1,0,0,0,0,1,0,0,1, % D
%   0,0,0,0,0,0,0,0,1,0, % E
%   0,0,0,0,0,0,0,1,0,0, % F
%   0,1,0,0,0,0,0,0,0,0, % G
%   0,1,0,0,0,1,0,0,0,0, % H
%   0,0,0,0,0,0,0,0,0,0, % I
%   0,0,0,0,0,0,0,1,0,1, % J
% ]
% 
% Question: Which two drop locations will minimize the total distance 
% that all villagers must travel?
% """
% 

% Solution:
% 
% total_dist: 178 (sum of the squared distances)
% 
% x: [4, 3, 5, 9]
%  0   0   0   0  13   0   0   0   0   0  
%  0   0   0   0   8   0   0   0   9  10  
%  5   0   0   0   0  10   0   5   4   5  
%  0   1   0P  0   0   0   5   0   0   2  
%  0   2   0   0   0   0   0   0   0P  0  
%  0   0   0   0   0   0   0   2   0   0  
%  0  10   0   0   0   0   0   0   0   0  
%  0  17   0   0   0  18   0   0   0   0  
%  0   0   0   0   0   0   0   0   0   0  
%  0   0   0   0   0   0   0  26   0  26  


% Note: the are two solutions (by symmetry), the other 
% one is x: [5, 9, 4, 3]
% i.e. where the cells are swapped. The latter
% solution has been removed by symmetry breaking.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;
array[1..n, 1..n] of int: huts;

% decision variables

% the two cells to drop the packages
array[1..2, 1..2] of var 1..n: x;

% all the distances (squared) from each cell to 
% the nearest package
array[1..n, 1..n] of var 0..n*n: distances;


% total distance
var int: total_dist;

% solve satisfy;
% solve minimize total_dist;
solve :: int_search(
        [x[i,j] | i,j in 1..2], 
        first_fail,
        indomain_split,
        complete) 
    % satisfy;
    minimize total_dist;

% 
% Calculate the distance between two cells.
% 
% Note: d is the _squared_ distance since I want to use 
% var int (and there is no support sqrt for var float).
% This doesn't matter here anyway.
% 
predicate dist(var int: i1, var int: j1, 
               int: i2, int: j2, var int: d) =
      d = abs(i1-i2)*abs(i1-i2) +
          abs(j1-j2)*abs(j1-j2)
;

% for solve satisfy
% constraint total_dist = 178;

constraint
   forall(i, j in 1..n where huts[i,j] = 1) (
      % check the distances to the two package cells
      % and pick the nearest.
      let {
        var 0..n*n: dist1,
        var 0..n*n: dist2
      } in
      dist(x[1,1], x[1,2], i, j, dist1) /\
      dist(x[2,1], x[2,2], i, j, dist2) /\

      % assign to the nearest package cell
      distances[i,j] = min([dist1, dist2]) 
   )

   /\ % assign 0 distance to cells with no hut
   forall(i, j in 1..n where huts[i,j] = 0) (
     distances[i,j] = 0
   )

   /\ % the total dist
   total_dist = sum([distances[i,j] | i,j in 1..n])
   /\
   total_dist >= 0

   /\ % symmetry breaking
   x[1,1] <= x[2,1]
;

output [
  "total_dist: " ++ show(total_dist) ++ "\n" ++
  "x: " ++ show(x) 
]
++
[ if j = 1 then "\n" else "" endif ++
    show_int(2, distances[i,j]) ++ 
    if fix(x[1,1]) = i /\ fix(x[1,2]) = j \/
       fix(x[2,1]) = i /\ fix(x[2,2]) = j
    then 
       "P " 
    else 
       "  "
    endif
  | i,j in 1..n
]
++ % Calculate the real total distance
[ "\nThe real total distance is "] ++
[
  show(sum([sqrt(int2float(fix(distances[i,j]))) | i,j in 1..n]))
]
++ ["\n"]
;


huts = array2d(1..n, 1..n, 
[
% 1 2 3 4 5 6 7 8 9 10
  0,0,0,0,1,0,0,0,0,0, % A
  0,0,0,0,1,0,0,0,1,1, % B
  1,0,0,0,0,1,0,1,1,1, % C
  0,1,0,0,0,0,1,0,0,1, % D
  0,1,0,0,0,0,0,0,1,0, % E
  0,0,0,0,0,0,0,1,0,0, % F
  0,1,0,0,0,0,0,0,0,0, % G
  0,1,0,0,0,1,0,0,0,0, % H
  0,0,0,0,0,0,0,0,0,0, % I
  0,0,0,0,0,0,0,1,0,1, % J
]);