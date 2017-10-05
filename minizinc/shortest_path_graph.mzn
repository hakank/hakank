% 
% Shortest path problem (graph representation) in MiniZinc.
% 
% Example from the 3 jugs problem (see 3_jugs.mzn or 3_jugs2.mzn).


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n;
array[1..n] of var 0..n: x;
int: num_edges;
array[1..num_edges, 1..2] of 1..n: graph;

int: start;
int: target;

% decision variables
var int: path_length = sum(i in 1..n) (x[i] > 0);

%
% all values are different (range 1..n) or 0
%
predicate all_different_except_0(array[int] of var int: x) =
  let {
      int: n = length(x)
   }
   in
   forall(i,j in 1..n where i != j) (
        (x[i] > 0 /\ x[j] > 0) -> x[i] != x[j]
   )
;


%
% y is the last element which is > 0. All elements after y is 0
%
predicate last_not_0(array[int] of var int: x, var int: y) =
   exists(i in 1..length(x)) (
      x[i] = y
      /\ % all elements in x after y are 0
      forall(j in i+1..n) (x[j] = 0)
      /\ % all elements in x before y are > 0
      forall(j in 1..i) (x[j] > 0)
   )
;

%
% all_paths
% 
% (Well, all paths are generated only if the solver generates all
%  possible solutions.)
%
predicate all_paths(array[int] of var int: x, 
                    array[int, 1..2] of var int: graph) =
   forall(i in 2..length(x)) (
     x[i] = 0
     \/
     (
       x[i] > 0
       /\
       x[i-1] > 0
       /\
       exists(j in index_set_1of2(graph)) (
         ( graph[j,1] = x[i-1] /\ graph[j,2] = x[i]   )
         \/
         ( graph[j,1] = x[i]   /\ graph[j,2] = x[i-1] )
       )
     )
   )
;


% solve satisfy;
% solve :: int_search(x, first_fail, indomain,complete) satisfy;
solve :: int_search(x, first_fail, indomain, complete) minimize path_length;


constraint
    all_different_except_0(x)
    /\
    x[1] = start
    /\
    last_not_0(x, target)
    /\
    all_paths(x, graph)

    % for generating all solutions when start = 1 and target = 15: path_length = 4
    % (which happens to be unique)
    % /\ path_length = 4
;


output [
  "path_length: \(path_length)\n: x: \([x[i] | i in 1..n where fix(x[i]) > 0])\n"
];

% directed graph
% n = 15;
% start = 1;
% target = 15;
% num_edges = 21;
% graph = array2d(1..num_edges, 1..2, 
%    [
%       1,2,
%       1,9,
%       2,3,
%       3,4,
%       3,9,
%       4,5,
%       5,6,
%       5,9,
%       6,7,
%       7,8,
%       7,9,
%       8,15,
%       9,10,
%      10,2,
%      10,11,
%      11,12,
%      12,2,
%      12,13,
%      13,14,
%      14,2,
%      14,15,
%    ]);

% undirected graph (just testing)
n = 15;
num_edges = 42;
start = 1;
target = 15;
graph = array2d(1..num_edges, 1..2, 
   [
      1,2, 2,1,
      1,9, 9,1,
      2,3, 3,2,
      3,4, 4,2,
      3,9, 9,3,
      4,5, 5,4,
      5,6, 6,5,
      5,9, 9,5,
      6,7, 7,6,
      7,8, 8,7,
      7,9, 9,7,
      8,15, 15,8,
      9,10, 10,9,
     10,2, 2,10,
     10,11, 11,10,
     11,12, 12,11,
     12,2,  2,12,
     12,13, 13,12,
     13,14, 14,13,
     14,2,  2,14,
     14,15, 15,14,

   ]);


