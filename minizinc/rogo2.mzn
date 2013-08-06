% 
% Rogo problem in MiniZinc.
% 
% From Mike Trick:
% "Operations Research, Sudoko, Rogo, and Puzzles"
% http://mat.tepper.cmu.edu/blog/?p=1302
% """
% [T]he goal is to find a loop through a grid of fixed length that contains 
% as many reward points as possible.
% """
% 
% See: http://www.rogopuzzle.co.nz/
% 
% Here I assume that there is a path of lenth max_tour.
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: W = 0; % white (empty) cells
int: B = -1; % black cells

int: max_val = max([problem[i,j] | i in 1..rows, j in 1..cols]);

% define the problem
int: rows;
int: cols;
int: max_steps; % max length of the loop
array[1..rows, 1..cols] of int: problem; 

% the coordinates in the path
array[1..max_steps] of var 1..rows: x;
array[1..max_steps] of var 1..cols: y;

% the collected points
int: max_point = max([problem[i,j] | i in 1..rows, j in 1..cols]);
array[1..max_steps] of var 0..max_point : points;

% objective: sum of points in the path
int: max_sum = sum([problem[i,j] | i in 1..rows, j in 1..cols where problem[i,j] > 0]); 
var 0..max_sum: sum_points;

% solve satisfy;
solve maximize sum_points;
% solve :: int_search(x ++ y, first_fail, indomain_min, complete) maximize sum_points;

% all coordinates must be unique
constraint
   forall(s in 1..max_steps, t in s+1..max_steps) (
         x[s] != x[t] \/ y[s] != y[t]
  )
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
      problem[x[s],y[s]] != B 
   )
;

% get the path
constraint 
  forall(s in 1..max_steps-1) (
     abs(x[s] - x[s+1]) + abs(y[s] - y[s+1]) = 1
  )
  /\ % close the path around the corner
  abs(x[max_steps] - x[1]) + abs(y[max_steps] - y[1]) = 1
;

% symmetry breaking: the cell with lowest coordinates
% should be in the first step.
% constraint
%       forall(i in 2..max_steps) (
%         x[1]*cols+y[1] < x[i]*cols+y[i]
%       )
% ;

% symmetry breaking:
% second step is larger than first step
constraint 
  x[1]*cols+y[1] < x[2]*cols+y[2]
;


output [
  "x     : " ++ show(x) ++ "\n" ++
  "y     : " ++ show(y) ++ "\n" ++
  "points: " ++ show(points) ++ "\n" ++
  "sum_points: " ++ show(sum_points) ++ "\n"

] ++ ["\n"] 
++
[
 "(" ++ show(x[i]) ++ ", " ++ show(y[i]) ++ "): " ++ show(points[i]) ++ if fix(points[i]) == 1 then " point" else " points" endif ++ "\n"

 | i in 1..max_steps
] ++ ["\n"];