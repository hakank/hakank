% 
% K4P2 Graceful Graph in MiniZinc.
% 
% Ported from Minion/Tailor summer_school/examples/K4P2GracefulGraph.eprime
%
% There are 1440 solutions.
%
% Cf K4P2GracefulGraph2.mzn for a more general model of this problem.

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

array[1..8] of var 0..16: nodes;
array[1..16] of var 1..16: edges;



solve :: int_search(nodes, first_fail, indomain_min, complete) satisfy;

constraint

abs(nodes[1] - nodes[2]) = edges[1] /\ 
abs(nodes[1] - nodes[3]) = edges[2] /\ 
abs(nodes[1] - nodes[4]) = edges[3] /\ 
abs(nodes[2] - nodes[3]) = edges[4] /\ 
abs(nodes[2] - nodes[4]) = edges[5] /\ 
abs(nodes[3] - nodes[4]) = edges[6] /\ 

abs(nodes[5] - nodes[6]) = edges[7] /\ 
abs(nodes[5] - nodes[7]) = edges[8] /\ 
abs(nodes[5] - nodes[8]) = edges[9] /\ 
abs(nodes[6] - nodes[7]) = edges[10] /\ 
abs(nodes[6] - nodes[8]) = edges[11] /\ 
abs(nodes[7] - nodes[8]) = edges[12] /\ 

abs(nodes[1] - nodes[5]) = edges[13] /\ 
abs(nodes[2] - nodes[6]) = edges[14] /\ 
abs(nodes[3] - nodes[7]) = edges[15] /\ 
abs(nodes[4] - nodes[8]) = edges[16] /\ 

all_different(edges) /\ 
all_different(nodes)
;

output
[
  "nodes: " ++ show(nodes) ++ "\n" ++
  "edges: " ++ show(edges) ++ "\n"
];
