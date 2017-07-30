% 
% Bridges to Somewhere problem in MiniZinc.
% 
% Problem from PuzzlOR
% Bridges to Somewhere (August 2009)
% http://puzzlor.editme.com/bridges
% """
% 
% The five residents of Hometown live in houses represented by the 
% letters "A" through "E" as shown on the left side of Figure 1. The offices 
% where they will be working are represented by their matching letters on the island of Worktown.
% 
% Because a river lies between Hometown and Worktown, the residents are unable 
% to get to work. They have in their budget enough funds to build two bridges 
% that could connect Hometown to Worktown. The locations where these bridges could 
% be built are indicated by the brown 1x3 hashed tiles. The two bridges can 
% only be built in these approved areas.
% 
% Once the bridges are built, the residents would then be able to commute to 
% work. A commuter will always take the shortest path from home to work and can 
% only travel in up, down, left or right directions (no diagonals). Each tile 
% represents a 1-km-by-1-km distance. As an example, if bridge four were built, 
% resident "E" would have to travel 10 km to reach his workplace.
% 
%    
%   Hometown     Bridge  Worktown
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 1
%    0,0,0,A,0,  1,1,1,  0,B,0,0,0, % 2
%    0,B,0,0,0,  0,0,0,  0,0,0,0,0, % 3
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 4
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 5
%    0,0,C,0,0,  0,0,0,  0,0,0,A,0, % 6
%    0,D,0,0,0,  2,2,2,  0,0,0,0,0, % 7
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 8
%    0,0,0,0,0,  0,0,0,  0,C,0,0,0, % 9
%    0,E,0,0,0,  3,3,3,  0,0,0,0,0, % 10
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 11
%    0,0,0,0,0,  4,4,4,  0,E,0,D,0, % 12
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 13
%

% 
% Question:
% Which two bridges should be built in order to minimize the total commuting 
% distance of all residents?
% """

% 
% Solution (which is unique modulo symmetry)
% 
%   tot_dist: 58
%   bridges: [1, 3]

%   Distances:
%   A: 12 using bridge 1 (at row 2)
%   B: 9 using bridge 1 (at row 2)
%   C: 12 using bridge 2 (at row 10)
%   D: 15 using bridge 2 (at row 10)
%   E: 10 using bridge 2 (at row 10)


% Extra: If we where allowed to build 2 bridges at
%        any place, then the optimal value is still 58
%        and the places are (up to symmetry) the following
%        positions (rows)
%          [2, 9]
%          [3, 9]
%          [2, 10]
%          [3, 10]
%
% 
% Extra 2: We can do a little better if we build 3 bridges
%          with a total distance 56 km.
% 
%   tot_dist: 56
%   bridges: [1, 2, 3]
%
%   Distances:
%   A: 12 using bridge 1 (at row 2)
%   B: 9 using bridge 1 (at row 2)
%   C: 10 using bridge 2 (at row 7)
%   D: 15 using bridge 2 (at row 7)
%   E: 10 using bridge 3 (at row 10)
% 
%   There is an alternative solution using bridge 1, 2, and 4
%   tot_dist: 56
%   bridges: [1, 2, 4]
% 
%   Distances:
%   A: 12 using bridge 1 (at row 2)
%   B: 9 using bridge 1 (at row 2)
%   C: 10 using bridge 2 (at row 7)
%   D: 15 using bridge 2 (at row 7)
%   E: 10 using bridge 3 (at row 12)
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_p = 5; % number of people
int: num_b = 4; % number of bridges
% int: num_b = 13; % number of bridges (extra: build at bridge at any place)

% How many bridges to build
int: num_bridges_to_build = 2;

% position of home/work
array[1..num_p, 1..4] of int: pos;

% x-coordinate of the bridges
array[1..num_b] of int: bridges;

% distances from home -> bridges (first cell)
array[1..num_p, 1..num_b] of int: dist1 = 
  array2d(1..num_p, 1..num_b, 
  [
    abs(pos[p,1] - bridges[b])+
    abs(pos[p,2] - 6)

    | p in 1..num_p, b in 1..num_b
  ]);

% distances from bridges (last cell) -> work
array[1..num_p, 1..num_b] of int: dist2 = 
  array2d(1..num_p, 1..num_b, 
  [
    abs(pos[p,3] - bridges[b])+
    abs(pos[p,4] - -1)
    | p in 1..num_p, b in 1..num_b
  ]);

array[1..num_p] of  string: str;

%
% decision variables
% 
% which bridges to build
array[1..num_bridges_to_build] of var 1..num_b: x;

% which is the nearest bridge
array[1..num_p] of var 1..num_b: b_ix;

% total distance
var int: tot_dist;

% solve satisfy;
solve minimize tot_dist;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


% I assume that the indices are ordered and unique
predicate min_index(var int: mi, array[int] of var int: x) =
  exists(i in index_set(x)) (
     x[i] = min(x) % minimum(x[i], x)
     /\
     mi = i 
  )
;

constraint

  % % output of the two dists matrices
  forall(p in 1..num_p) (
    trace(show([dist1[p,b] | b in 1..num_b]) ++ " -> " ++  
          show([dist2[p,b] | b in 1..num_b]) ++
          "\n", 1=1)
  )
  /\

  tot_dist = sum(p in 1..num_p) (
                min(
                   [dist1[p,x[b]] + 1 + dist2[p,x[b]] | b in 1..num_bridges_to_build]
                )
             )

  /\ % which bridge is the nearest (as index in x) ?
  forall(p in 1..num_p) (
    let {
      var 1..num_b: min_b_ix
    } in
       min_index(min_b_ix, [dist1[p,x[b]] + 1 + dist2[p,x[b]] | b in 1..num_bridges_to_build])
       /\
       b_ix[p] = min_b_ix
  )

  % /\ x[1] < x[2] % symmetry breaking
  /\ alldifferent(x)
  /\ increasing(x)
;




% for solve satisfy
% constraint tot_dist = 58;
% constraint tot_dist = 56; % for 3 bridges

output [
  "tot_dist: " ++ show(tot_dist) ++ "\n" ++
  "bridges: " ++ show(x) ++ "\n" ++ "\n" ++
  "Distances:\n"
]
++
% [
%   show(str[p]) ++ ": " ++
%   if 
%      fix(dist1[p,x[1]] + 1 + dist2[p,x[1]]) < 
%      fix(dist1[p,x[2]] + 1 + dist2[p,x[2]])
%   then 
%      show(dist1[p,x[1]]) ++ " + " ++ show(dist2[p,x[1]]) ++ " + 1 (bridge " ++ show(x[1]) ++ ") "
%   else 
%      show(dist1[p,x[2]]) ++ " + " ++ show(dist2[p,x[2]]) ++ " + 1 (bridge " ++ show(x[2]) ++ ") "
%   endif
%   ++ " = total distance: " ++
%   show(fix(min(
%      dist1[p,x[1]] + 1 + dist2[p,x[1]],
%      dist1[p,x[2]] + 1 + dist2[p,x[2]])
%   )) ++ "\n"
%   | p in 1..num_p
% ]
[
   % let {
   %    array[1..num_bridges_to_build] of var int: bb = [dist1[p,x[b]] + 1 + dist2[p,x[b]] | b in 1..num_bridges_to_build],
   %    var int: min_b = min(bb)
   % }
  let {
      array[1..num_bridges_to_build] of int: bb = [fix(dist1[p,fix(x[b])]) + 1 + fix(dist2[p,x[b]]) | b in 1..num_bridges_to_build],
      int: min_b = min(bb)
   }
   in
   show(str[p]) ++ ": " ++
   show(fix(min_b)) ++ " using bridge " ++ show(b_ix[p]) ++ " (at row " ++ show(bridges[x[b_ix[p]]]) ++ ")"
   ++ "\n"
   | p in 1..num_p
]
++ ["\n"]
;



%
% Data
%

%    1 2 3 4 5           1 2 3 4 5
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 1
%    0,0,0,A,0,  1,1,1,  0,B,0,0,0, % 2
%    0,B,0,0,0,  0,0,0,  0,0,0,0,0, % 3
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 4
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 5
%    0,0,C,0,0,  0,0,0,  0,0,0,A,0, % 6
%    0,D,0,0,0,  2,2,2,  0,0,0,0,0, % 7
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 8
%    0,0,0,0,0,  0,0,0,  0,C,0,0,0, % 9
%    0,E,0,0,0,  3,3,3,  0,0,0,0,0, % 10
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 11
%    0,0,0,0,0,  4,4,4,  0,E,0,D,0, % 12
%    0,0,0,0,0,  0,0,0,  0,0,0,0,0, % 13

pos = array2d(1..num_p, 1..4,
[
% home work
  2,4,  6,4,  % A 
  3,2,  2,2,  % B
  6,3,  9,2,  % C
  7,2, 12,4,  % D
 10,2, 12,2   % E
]);

% x coordinates for the bridges
bridges = [2,7,10,12];

% Testing: what if we can build 2 bridges at any places?
% x coordinates for the bridges
% bridges = [1,2,3,4,5,6,7,8,9,10,11,12,13];

str = ["A","B","C","D","E"];