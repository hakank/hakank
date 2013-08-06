% 
% K4P2 Graceful Graph in MiniZinc.
% 
% Problem from Minion summer_school/examples/K4P2GracefulGraph.eprime
% 
% This is a more general model than K4P2GracefulGraph.mzn
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: m = 16;
int: n = 8;
array[1..n] of var 0..m: nodes;
array[1..m, 1..2] of var 1..n: graph;
array[1..m] of var 1..m: edges;



solve :: int_search(nodes, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i in 1..m) (
    abs(nodes[graph[i,1]] - nodes[graph[i,2]]) = edges[i]  
  )
  /\
  all_different(edges) 
  /\ 
  all_different(nodes)
;

graph =
array2d(1..m, 1..2,
[1, 2,
1, 3,
1, 4,
2, 3,
2, 4,
3, 4,

5, 6,
5, 7,
5, 8,
6, 7,
6, 8,
7, 8,

1, 5,
2, 6,
3, 7,
4, 8]);

output
[
  "nodes: " ++ show(nodes) ++ "\n" ++
  "edges: " ++ show(edges) ++ "\n"
];
