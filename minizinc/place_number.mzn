% 
% Place number puzzle in MiniZinc.
%
% http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
% """
% Place numbers 1 through 8 on nodes
% - each number appears exactly once
% - no connected nodes have consecutive numbers
%      2 - 5 
%    / | X | \
%  1 - 3 - 6 - 8
%    \ | X | /
%      4 - 7
% """
% 
% Note: This is the same problem as NumberEightPuzzle.mzn
%

% Two solutions (plus their reverses):
% x = [2, 5, 8, 6, 3, 1, 4, 7]
% 
% x = [2, 6, 8, 5, 4, 1, 3, 7]


% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 8;
array[1..n] of var 1..8: x;

int: m = 32;
array[1..m, 1..2] of 1..8: graph;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x)
  /\
  forall(i in 1..m) (
     abs(x[graph[i,1]]-x[graph[i,2]]) > 1 
  )
  /\ % symmetry breaking
  x[1] < x[n]
;

output [
  show(x), "\n"
  
];

graph = [|
  1,2,
  |1,3,
  |1,4,
  |2,1,
  |2,3,
  |2,5,
  |2,6,
  |3,2,
  |3,4,
  |3,6,
  |3,7,
  |4,1,
  |4,3,
  |4,6,
  |4,7,
  |5,2,
  |5,3,
  |5,6,
  |5,8,
  |6,2,
  |6,3,
  |6,4,
  |6,5,
  |6,7,
  |6,8,
  |7,3,
  |7,4,
  |7,6,
  |7,8,
  |8,5,
  |8,6,
  |8,7
|];