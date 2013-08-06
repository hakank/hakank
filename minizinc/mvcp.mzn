% 
% Minimum Vertex Cover Problem in MiniZinc.
% 
% From GLPK:s example mvcp.mod
% """
% MVCP, Minimum Vertex Cover Problem 
%
% Written in GNU MathProg by Andrew Makhorin <mao@gnu.org>
%
% The Minimum Vertex Cover Problem in a network G = (V, E), where V
% is a set of nodes, E is a set of arcs, is to find a subset V' within
% V such that each edge (i,j) in E has at least one its endpoint in V'
% and which minimizes the sum of node weights w(i) over V'.
%
% Reference:
% Garey, M.R., and Johnson, D.S. (1979), Computers and Intractability:
% A guide to the theory of NP-completeness [Graph Theory, Covering and
% Partitioning, Minimum Vertex Cover, GT1].
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_nodes;

int: num_edges;
array[1..num_edges,1..2] of 1..num_nodes: E;

% x[i] = 1 means that node i is included into V' 
array[1..num_nodes] of var 0..1: x;


% w[i] is weight of vertex i 
array[1..num_nodes] of int: w = [1 | i in 1..num_nodes];

% we need to minimize the sum of node weights over V' 
var int: z = sum(i in 1..num_nodes) (w[i]*x[i]);

solve minimize z;

constraint
   % each edge (i,j) must have node i or j (or both) in V' 
   forall(i in 1..num_edges) (
       x[E[i,1]] + x[E[i,2]] >= 1
   )
;


%
% data
% 

% """
% These data correspond to an example from [Papadimitriou].
% 
% Optimal solution is 6 (greedy heuristic gives 13)
% """

num_nodes = 19;
num_edges = 27;


int: a1 = 1;
int: a2 = 2;
int: a3 = 3;
int: a4 = 4;
int: a5 = 5;
int: a6 = 6;
int: a7 = 7;
int: b1 = 8;
int: b2 = 9;
int: b3 = 10;
int: b4 = 11;
int: b5 = 12;
int: b6 = 13;
int: c1 = 14;
int: c2 = 15;
int: c3 = 16;
int: c4 = 17;
int: c5 = 18;
int: c6 = 19;

E = array2d(1..num_edges, 1..2, 
[
a1,b1,
b1,c1,
a1,b2,
b2,c2,
a2,b3,
b3,c3,
a2,b4,
b4,c4,
a3,b5,
b5,c5,
a3,b6,
b6,c6,
a4,b1,
a4,b2,
a4,b3,
a5,b4,
a5,b5,
a5,b6,
a6,b1,
a6,b2,
a6,b3,
a6,b4,
a7,b2,
a7,b3,
a7,b4,
a7,b5,
a7,b6]);


output
[
 "x: " ++ show(x) ++ "\n" ++
 "z: " ++ show(z) 
];