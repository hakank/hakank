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
% This version (compared to http://www.hakank.org/minizinc/rogo2.mzn ) has:
% - the connection pairs are now calculated (as int, 
%   i.e. not dynamically with decision vars) awhich 
% - a table constraint uses these connection pairs matrix
% - integer variable "best" is added to the problem instance.
%   This model now constrain "sum_points" to be less or equal this value
% - another way of assuring unicity of the path
% - domain annotations for some of the global constraints.
% - Some comments how Gecode/fz and Chuffed do for some of the 
%   constraints/labeling/annotations.
%   Gecode/fz and Chuffed reacts somewhat different on these.
%
% - Chuffed seems to be best with these parameters
%    -vsids=true -mdd=true

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% For Gecode/fz when using afc_min/indomain_min
% include "gecode.mzn";

int: W = 0; % white (empty) cells
int: B = -1; % black cells

int: max_val = max([problem[i,j] | i in 1..rows, j in 1..cols]);

% define the problem
int: rows;
int: cols;
int: max_steps; % max length of the loop
array[1..rows, 1..cols] of int: problem; 
int: best; % best known value


% array[1..rows*cols*3*3] of int: valid_path = 
%    [ 
%      if i+a > 0  /\ j+b > 0 /\ i+a <= rows /\ j+b <= cols then 1+(i+a-1)*cols+(j+b-1) else -1 endif 
%      | i in 1..rows, j in 1..cols, a, b in {-1,0,1} 
%    ];

% var 0..rows*cols*rows*cols: valid_connections1_counted;

%%
%% This is a rows*cols x rows*cols _matrix_ for all the 
%% valid connections (on a - non-torus - von Neumann grid)
% array[1..rows*cols, 1..rows*cols] of int: valid_connections1 = 
%    array2d(1..rows*cols, 1..rows*cols,
%    [ 
%      if (abs(j1-j2) = 1  /\ i1 = i2) \/ (abs(i1-i2) = 1 /\ j1 mod cols = j2 mod cols)  
%      then 
%         1 
%       else 
%         0 
%      endif 
%      | i1 in 1..rows, j1 in 1..cols, i2 in 1..rows, j2 in 1..cols
%    ]);


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

% the collected points
int: max_point = max([problem[i,j] | i in 1..rows, j in 1..cols]);
array[1..max_steps] of var 0..max_point : points;

% objective: sum of points in the path
% int: max_sum = sum([problem[i,j] | i in 1..rows, j in 1..cols where problem[i,j] > 0]); 
var 0..best: sum_points;

% solve satisfy;
% Gecode/fz (without ::domain on alldifferent) 5.4 seconds (55494) when using this on intro8.
solve maximize sum_points; 
% For Gecode/fz with afc_min/interval_min: 4 seconds (63139 failures) for intro8
% solve :: int_search(x ++ y, afc_min, indomain_min, complete) maximize sum_points; % Gecode
% solve :: int_search(x ++ y, first_fail, indomain_min, complete) maximize sum_points;
% solve :: int_search(x ++ y, occurrence, indomain_min, complete) maximize sum_points;

constraint
  trace("best: " ++ show(best) ++ "\n", 1=1)
;

% all coordinates must be unique (first approach)
% Note: Sometimes it's faster if this is also active.
constraint
   forall(s in 1..max_steps, t in s+1..max_steps) (
         x[s] != x[t] \/ y[s] != y[t]
  )
;

% all coordinates must be unique
% using alldifferent instead
constraint
   % Without ":: domain" Gecode/fz solves intro8 in 4s (afc_min/indomain_min) 
   % (without: much longer)
   % With :: domain Chuffed solves intro8 in 3.5s (without: in 36 seconds)
   alldifferent([(x[i]-1)*max_steps + y[i] | i in 1..max_steps]) % :: domain
;

% check with best number of points
constraint sum_points <= best;

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
% Keeping this seems to boost the performance:
%   with: Chuffed 3.3 seconds, without: 6.8 seconds
constraint 
  forall(s in 1..max_steps-1) (
     abs(x[s] - x[s+1]) + abs(y[s] - y[s+1]) = 1
  )
  /\ % close the path around the corner
  abs(x[max_steps] - x[1]) + abs(y[max_steps] - y[1]) = 1
;

% % Only valid connections using a full rows*cols x rows*cols matrix.
% % This don't propagate very well in this model.
% constraint
%   forall(s in 1..max_steps-1) (
%     valid_connections1[(x[s]-1)*cols+y[s], (x[s+1]-1)*cols+y[s+1]] = 1
%   )
%   /\ % "around the corner
%   valid_connections1[(x[max_steps]-1)*cols+y[max_steps], (x[1]-1)*cols+y[1]] = 1
% ;

%
% Only valid connections, using table/2.
%
constraint
  forall(s in 1..max_steps-1) (
    table([(x[s]-1)*cols+y[s], (x[s+1]-1)*cols+y[s+1]], valid_connections) :: domain
  )
  /\ % "around the corner
  table([(x[max_steps]-1)*cols+y[max_steps], (x[1]-1)*cols+y[1]], valid_connections) :: domain
;


% Without these two symmetry breaking, Chuffed solves Intro 8 problem
% in 2 seconds (5919 conflicts). With them: 3.4 seconds (9481 conflicts).
% And:
%   with :: domain on table and :: domain on alldifferent: 2 seconds (6014 conflicts)
%
% Symmetry breaking: the cell with lowest coordinates should be in the first step.
% Without this (but including the second): Chuffed solves Intro 8 in 0.7s (3525 conflicts)
%
constraint
   forall(i in 2..max_steps) (
     (x[1]-1)*cols+y[1] < (x[i]-1)*cols+y[i]
   )
;

% Symmetry breaking:
% Second step is larger than first step
constraint 
  (x[1]-1)*cols+y[1] < (x[2]-1)*cols+y[2]
;


%
% output
%
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
]
++
["\n"];