% 
% Monorail puzzle in MiniZinc.
% 
% From Aaron Iba's Blog:
% http://blog.aaroniba.net/2011/07/06/a-lesson-from-my-ios-users-they-dont-teach-at-mit/#comments
% """
% The object of Monorail puzzles is to complete a closed-circuit 
% loop through all the stations (dots) by drawing rails.  The loop 
% must pass through each station exactly once and close back on itself, 
% like an actual monorail system might in a city.
%
% Problem:
%
%     .   .   .   .       1  2  3  4
% 
%     .   .___.   .       5  6  7  8
%         |
%     .   .   .   .       9 10 11 12
% 
%     .   .___.   .      13 14 15 16
%  
% Solution:
%     .__.___.___.
%     |          |
%     .  .___.___.
%     |  | 
%     .  .___.___.
%     |          |
%     .__.___.___.
% 
% """
%
% Also see: Glenn Iba's site:
%   - http://glenniba.com/index.html
%   - Monorail Support Page: http://glenniba.com/monorailsupport.html
%   - The introduction to Glenn Iba's book "Round Trip Puzzles":
%     http://glenniba.com/Intro-long-web.pdf
%   - Glenn Iba "Hamiltonian Cycle Puzzles" (paper)
%     http://glenniba.com/G4G8%20exchange%20paper.pdf
% 
% My first version is to use the same approach as in the Rogo model.
%    http://www.hakank.org/minizinc/rogo3.mzn
%
% This model can yet only handle normal rectangular matrices.
% TODO: Handle romboids etc as well...
%
%
% Chuffed seems to be best with the following:
%    -mdd=true -vsids=true --free
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
% include "gecode.mzn";
% include "jacop.mzn";

%
% define the problem
%

% From the problem instance:
int: rows;
int: cols;
% number of given hints
int: num_given;
% problem states as integers 1..rows*cols
array[1..num_given, 1..2] of int: given;

int: num_steps = rows*cols; % length of the path


% The number of valid connections (on a non-torus von Neumann grid) are:
%  4*2                         +   %  the 4 corners has 2 connections each
%  3*(2*(rows-2) + 2*(cols-2)  +   %  the 4 borders has 3 connections
%  4*(rows-2)*(cols-2)         +   %  all the rest has 4 connections
int: num_connections = 4*2 + 3*(2*(rows-2) + 2*(cols-2)) + 4*(rows-2)*(cols-2);


%
% The valid connections as pairs of integers 1..rows*cols.
%
% Note: The model might have to convert to coordinates (i,j):
%   num <=> ((i-1)*cols+j)
%
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



%
% decision variables: the coordinates in the path
%
array[1..num_steps] of var 1..rows: x;
array[1..num_steps] of var 1..cols: y;

% the path as integers
array[1..num_steps] of var 1..num_steps: path;

% For diffn/4.
array[1..num_steps] of int: a = [1 | i in 1..num_steps];


% solve satisfy;
% input_order/indomain_min is much faster for Gecode/fz, and slighly faster for Chuffed
% solve :: int_search(path, input_order, indomain_min, complete) satisfy;
solve :: int_search(path, input_order, indomain_min, complete) satisfy;


%
% populate the given hints
%
constraint
   forall(k in 1..num_given) (
       let {
          var 1..num_steps: a,
          var 1..num_steps: b
       } in 
       path[a] = given[k,1] /\ path[b] = given[k,2]
       /\
       (abs(a-b) = 1 \/ abs(a-b) = num_steps-1)
   )
;


% all coordinates must be unique (first approach)
% Note: Sometimes it's faster if this is also active.
% constraint
%   forall(s in 1..num_steps, t in s+1..num_steps) (
%          (x[s] != x[t]) \/ (y[s] != y[t])
%   )
%   % /\ diffn(x,y,a,a) % experiment
% ;

%
% All coordinates must be unique, 
%
constraint
   alldifferent(path) :: domain
;

%
% Only valid connections, using table.
%
constraint
  forall(s in 1..num_steps-1) (
    table([path[s], path[s+1]], valid_connections) :: domain
  )
  /\ % "around the corner
  table([path[num_steps], path[1]], valid_connections) :: domain
;


%
% Channel between path and coordinate representation.
%
% (I tested to convert the path to a circuit, but it was slower.)
%
constraint
   forall(i in 1..num_steps) (
     (x[i]-1)*cols + y[i] = path[i] 
   )
;

% symmetry breaking
% Start with the two steps: 1,1  and 1,2.
%
constraint
  path[1] = 1
  /\
  path[2] = 2
;


%
% output
%
output [
  "x     : " ++ show(x) ++ "\n" ++
  "y     : " ++ show(y) ++ "\n"
] ++ ["\n"] 
++
[
 "(" ++ show(x[i]) ++ ", " ++ show(y[i]) ++ "): " ++ show(path[i]) ++ "\n"

 | i in 1..num_steps
]
++
[
  "path: " ++ show(path) ++ "\n"
]
++
["\n"];
