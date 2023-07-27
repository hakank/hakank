% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/C/bipartite.html
% """
% bipartite​(NODES)
% 
% Purpose
%  Consider a digraph G described by the NODES collection. Select a subset 
%  of arcs of G so that the corresponding graph is symmetric (i.e., if there 
%  is an arc from i to j, there is also an arc from j to i) and bipartite 
%  (i.e., there is no cycle involving an odd number of vertices).
% 
% Example
%     (
%     <
%     index-1 succ-{2, 3},
%     index-2 succ-{1, 4},
%     index-3 succ-{1, 4, 5},
%     index-4 succ-{2, 3, 6},
%     index-5 succ-{3, 6},
%     index-6 succ-{4, 5}
%     >
%     )
%
% The bipartite constraint holds since the NODES collection depicts a 
% symmetric graph with no cycle involving an odd number of vertices. 
% The corresponding graph is depicted by Figure 4.39.1.
% 
%  Figure 4.39.1. The bipartite graph associated with the example
%
%       Red(2)  <->  Blue(4) <-> Red(6)
%       ^              ^          ^
%       |              |          |
%       v              v          v
%       Blue(1) <->  Red(3)  <-> Blue(5)
% 
% """

% The the bipartite coloring is:
%  1 -> Blue
%  2 -> Red
%  3 -> Red
%  4 -> Blue
%  5 -> Blue
%  6 -> Red
% i.e. the partitioning set is [{1,4,5}, {2,3,6}]

%
% Note: this model just constructs a bipartition, and do not select a subgraph
% of the given graph.
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6;
array[1..n] of var set of 1..n: s; % the graph
array[1..2] of var set of 1..n: b; % the bipartite set


%
% check for symmetric graph
%
predicate symmetric(array[int] of var set of int: s) =

  forall(i, j in index_set(s) where i != j) (
    j in s[i] <-> i in s[j]
  )
;

%
% Converts an array of var set to a matrix of bool in order to
% loop through the values of the sets.
%
% Usage:
% set_matrix2boolean_matrix(set, boolean matrix, universe)
%
predicate set_matrix2boolean_matrix(array[int] of var set of int: s, array[int,int] of var bool: s_b, set of int: universe) =
   forall(i in universe) (
      link_set_to_booleans(s[i], [s_b[i,j] | j in universe])
   )
;

predicate cp1d(array[int] of set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


%
% Checks for a bipartite graph, i.e. a graph which can be colored
% in two colors and there is no connection between nodes of the same
% color.
%
predicate bipartite(
        array[int] of var set of int: s, 
        array[int] of var set of int: b,
        set of int: universe) =
   let {
      int: lbs = min(index_set(s)),
      int: ubs = max(index_set(s)),
      array[lbs..ubs, lbs..ubs] of var bool: s_bools
   }
   in
   symmetric(s)
   /\
   partition_set(b, universe)

   /\ % convert x to a boolean matrix
   set_matrix2boolean_matrix(s, s_bools, universe)

   /\ % if a node is "red" then all the connected nodes must be "blue"
      % and vice versa.  
   forall(i in universe) (
      (
       i in b[1] <-> forall(j in universe where i != j) (s_bools[i,j] -> j in b[2]) 
       )
      /\
      (
       i in b[2] <-> forall(j in universe where i != j) (s_bools[i,j] -> j in b[1]) 
       )
    )
   % /\ % symmetry breaking
   % this only works in MiniZinc/flatzinc, but not in ECLiPSe:s solvers or
   % Gecode/fz
   % b[1] < b[2] 

   /\ % an ersatz symmetry breaking: 
      % connections to the first node is in b[2]
   forall(j in universe) (
      s_bools[1,j] -> j in b[2]
   )
;

solve satisfy;

constraint

  % for generating bipartitions
    forall(i in 1..n) (
      not(i in s[i]) 
      /\ 
      card(s[i]) >= 2
    ) 
   % /\
   % cp1d([{1,2,3,4}, {5,6}], b)
%
%  /\
   % cp1d([
   %       {2,3},    % 1
   %       {1,4},    % 2
   %       {1,4,5},  % 3
   %       {2,3,6},  % 4
   %       {3,6},    % 5
   %       {4,5}     % 6
   %     ],s)

   /\
   bipartite(s, b, 1..n)
;


output [
  "s: ", show(s), "\n",
  "b: ", show(b), "\n",
];
