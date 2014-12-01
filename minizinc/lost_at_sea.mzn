% 
% Lost at Sea problem (PuzzlOr) in MiniZinc.
% 
% http://puzzlor.editme.com/lostatsea2 
% """
% Searching for a lost ship at sea is a time sensitive task that 
% requires skill and urgency.  Finding a lost ship quickly means the 
% difference between life and death.
% 
%   3,0,0,3,2,4,2,3,
%   3,3,3,1,2,4,1,4,
%   0,4,0,1,2,3,4,0,
%   1,1,0,3,4,1,1,0,
%   1,1,3,3,1,2,2,4,
%   0,2,3,3,3,0,2,4,
%   2,3,2,4,2,4,1,1,
%   2,1,2,2,2,4,1,3,
%
% The map in Figure 1 shows a section of ocean divided into 64 cells.  
% Somewhere in this grid, a ship has been lost.  Each cell has a number 
% that represents the probability of finding the lost ship when that cell 
% is searched (based on last known position, ocean currents, and debris 
% sightings).  For example, if you searched cell A1, you would have a 2% 
% chance of finding the lost ship there.  
%
% As the leader of the search and rescue team, your goal is to find the ship
% with all survivors.  Unfortunately it takes you 1 day to search a cell 
% and the lost sailors have only enough food and water to survive for 10 days.  
% This allows you to search a total of 10 cells before the lost sailors perish.
% 
% You may start your search in any of the 64 cells.  You are only allowed 
% to move to adjacent cells (you cannot move diagonally) and you are not 
% allowed to revisit any cells.  Add up the percentages in the 10 cells you 
% have searched to get the probability of finding the lost ship.
% 
% Question:  What is the greatest probability of finding the lost ship?
% """

% This is based on my Rogo model http://www.hakank.org/minizinc/rogo5.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 


int: W = 0; % white (empty) cells
int: B = -1; % black cells

int: max_val = max([problem[i,j] | i in 1..rows, j in 1..cols]);

% define the problem
int: rows = 8;
int: cols = 8;
int: max_steps = 10; % max length of the loop
array[1..rows, 1..cols] of int: problem =
array2d(1..rows, 1..cols, 
[3,0,0,3,2,4,2,3,
 3,3,3,1,2,4,1,4,
 0,4,0,1,2,3,4,0,
 1,1,0,3,4,1,1,0,
 1,1,3,3,1,2,2,4,
 0,2,3,3,3,0,2,4,
 2,3,2,4,2,4,1,1,
 2,1,2,2,2,4,1,3]);


int: best = 0; % best known value

% The number of valid connections (on a non-torus von Neumann grid) are:
%  4*2                         +   %  the 4 corners has 2 connections each
%  3*(2*(rows-2) + 2*(cols-2)  +   %  the 4 borders has 3 connections
%  4*(rows-2)*(cols-2)         +   %  all the rest has 4 connections
int: num_connections = 4*2 + 3*(2*(rows-2) + 2*(cols-2)) + 4*(rows-2)*(cols-2);


% The valid connections as pairs of integers 1..rows*cols.
% The model must convert to coordinates.
array[1..num_connections, 1..2] of int: valid_connections = 
   array2d(1..num_connections, 1..2,
   [ 
     if k = 1 then 
       (i1-1)*cols+j1
      else 
       (i2-1)*cols+j2
     endif 
     | i1 in 1..rows, j1 in 1..cols, i2 in 1..rows, j2 in 1..cols, k in 1..2
       where (abs(j1-j2) = 1  /\ i1 = i2) \/ (abs(i1-i2) = 1 /\ j1 mod cols = j2 mod cols)
   ])
;


% the coordinates in the path
array[1..max_steps] of var 1..rows: x;
array[1..max_steps] of var 1..cols: y;

array[1..max_steps] of var 1..rows*cols: path;

% the collected points
int: max_point = max([problem[i,j] | i in 1..rows, j in 1..cols]);
array[1..max_steps] of var 0..max_point : points;

% objective: sum of points in the path
int: max_sum = sum([problem[i,j] | i in 1..rows, j in 1..cols where problem[i,j] > 0]); 
int: max_best = if best > 0 then best else max_sum endif;
var 0..max_best: sum_points;


% solve satisfy;
solve :: int_search(path, input_order, indomain_min, complete) maximize sum_points;

constraint
  trace("best: " ++ show(best) ++ " max_best: " ++ show(max_best) ++ "\n", 1=1)
;

%
% channel between path (integer representation) and 
% the coordinate representation in (x, y)
%
constraint
   forall(i in 1..max_steps) (
     path[i] = (x[i]-1)*cols + y[i]
   )
;


% all coordinates must be unique (first approach)
% Note: Sometimes it's faster if this is also active.
% constraint
%    forall(s in 1..max_steps, t in s+1..max_steps) (
%          x[s] != x[t] \/ y[s] != y[t]
%   )
% ;

% all coordinates must be unique
% using alldifferent instead
constraint
   alldifferent(path) :: domain
;

% check with best number of points
constraint 
  if best > 0 then 
    sum_points = best
  else 
    true
  endif
;

% calculate the points (to maximize)
constraint 
   forall(s in 1..max_steps) (
      points[s] = problem[x[s], y[s]]
   )
   /\
   sum_points = sum(points)
;


% ensure that there are no black cells
% in the path
constraint 
   forall(s in 1..max_steps) (
      problem[x[s], y[s]] != B 
   )
;

%
% Get the path (coordinates)
%
constraint 
  forall(s in 1..max_steps-1) (
     abs(x[s] - x[s+1]) + abs(y[s] - y[s+1]) = 1
  )
  % /\ % close the path around the corner
  % abs(x[max_steps] - x[1]) + abs(y[max_steps] - y[1]) = 1
;


%
% Only valid connections, using table/2.
%
constraint
  forall(s in 1..max_steps-1) (
    % table([(x[s]-1)*cols+y[s], (x[s+1]-1)*cols+y[s+1]], valid_connections) :: domain
    % /\
    table([path[s], path[s+1]], valid_connections) :: domain
  )
  % /\ % "around the corner
  % table([(x[max_steps]-1)*cols+y[max_steps], (x[1]-1)*cols+y[1]], valid_connections) :: domain
  % /\
  % table([path[max_steps], path[1]], valid_connections) :: domain
;

%
% Symmetry breaking 1: 
% the cell with lowest coordinates should be in the first step.
%
constraint
   forall(i in 2..max_steps) (
     %(x[1]-1)*cols+y[1] < (x[i]-1)*cols+y[i]
     %/\
     path[1] < path[i]
   )
;

% Symmetry breaking:
% Second step should be larger than the first step
constraint 
  %(x[1]-1)*cols+y[1] < (x[2]-1)*cols+y[2]
  %/\
  path[1] < path[2]
;


%
% output
%
output [
  "x     : " ++ show(x) ++ "\n" ++
  "y     : " ++ show(y) ++ "\n" ++
  "points: " ++ show(points) ++ "\n" ++
  "sum_points: " ++ show(sum_points) ++ "\n" ++
  "path: " ++ show(path) ++ "\n"

] ++ ["\n"] 
++
[
 "(" ++ show(x[i]) ++ ", " ++ show(y[i]) ++ "): " ++ show(points[i]) ++ if fix(points[i]) == 1 then " point" else " points" endif ++ "\n"

 | i in 1..max_steps
]
++
["\n"];