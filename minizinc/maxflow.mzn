% 
% Maximum Flow Problem, integer programming in MiniZinc.
% 
% From GLPK:s example maxflow.mod
% """
% MAXFLOW, Maximum Flow Problem
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
%
% The Maximum Flow Problem in a network G = (V, E), where V is a set
% of nodes, E within V x V is a set of arcs, is to maximize the flow
% from one given node s (source) to another given node t (sink) subject
% to conservation of flow constraints at each node and flow capacities
% on each arc.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% number of nodes 
int: n;

% set of arcs 
int: num_edges;
array[1..num_edges, 1..2] of 1..n: E;

% a[i,j] is capacity of arc (i,j) 
array[1..num_edges] of int: a;

% source node 
1..n: s;

% sink node 
1..n: t;

% x[i,j] is elementary flow through arc (i,j) to be found 
array[1..num_edges] of var int: x;

% total flow from s to t 
var int: flow; %, >= 0;


% objective is to maximize the total flow through the network 
solve maximize flow;

constraint 
  flow >= 0
  /\
  flow <= 100
  /\
  forall(i in 1..num_edges) (
    x[i] >= 0
    /\
    x[i] <= a[i]
  )
  /\
  forall(i in 1..n) (
      % node[i] is conservation constraint for node i 
      % 
      % summary flow into node i through all ingoing arcs 
      sum(k in 1..num_edges where E[k,2] = i) (x[k]) + flow*bool2int(i = s)
      = % must equal 
      % summary flow from node i through all outgoing arcs 
      sum(k in 1..num_edges where E[k,1] = i) (x[k]) + flow*bool2int(i = t)
  )
;

%
% data
%

% """
% These data correspond to an example from [Christofides]. 
%
% Optimal solution is 29 
% """
n = 9;
s = 1;
t = n;

num_edges = 14;
E = array2d(1..num_edges, 1..2,
[
       1, 2, 
       1, 4,
       2, 3,
       2, 4,
       3, 5,
       3, 8,
       4, 5,
       5, 2,
       5, 6,
       5, 7,
       6, 7,
       6, 8,
       7, 9,
       8, 9]);

a = [14,23,10, 9,12,18,26,11,25, 4, 7, 8,15,20];

output
[
  "x: " ++ show(x) ++ "\n" ++
  "flow: " ++ show(flow)
];
