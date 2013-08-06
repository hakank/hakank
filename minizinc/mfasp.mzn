% 
% Minimum Feedback Arc Set Problem  in MiniZinc.
% 
% From GLPK:s example mfasp.mod:
% """
% MFASP, Minimum Feedback Arc Set Problem */
%
% Written in GNU MathProg by Andrew Makhorin <mao@gnu.org>
%
% The Minimum Feedback Arc Set Problem for a given directed graph
% G = (V, E), where V is a set of vertices and E is a set of arcs, is
% to find a minimal subset of arcs, which being removed from the graph
% make it acyclic.
%
% Reference:
% Garey, M.R., and Johnson, D.S. (1979), Computers and Intractability:
% A guide to the theory of NP-completeness [Graph Theory, Covering and
% Partitioning, Minimum Feedback Arc Set, GT9].
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% number of vertices 
int: n;

% set of arcs 
int: num_edges;
array[1..num_edges, 1..2] of 1..n: E;


% """
% x[i,j] = 1 means that (i->j) is a feedback arc 
% 
% It is known that a digraph G = (V, E) is acyclic if and only if its
% vertices can be assigned numbers from 1 to |V| in such a way that
% k[i] + 1 <= k[j] for every arc (i->j) in E, where k[i] is a number
% assigned to vertex i. We may use this condition to require that the
% digraph G = (V, E \ E'), where E' is a subset of feedback arcs, is
% acyclic. 
% """
array[1..num_edges] of var 0..1: x;

% k[i] is a number assigned to vertex i 
array[1..n] of var 1..num_edges: k;


% the objective is to minimize the cardinality of a subset of feedback
% arcs 
var int: obj = sum(i in 1..num_edges) (x[i]);
solve :: int_search(
          [x[i] | i in 1..num_edges] ++
          [k[i] | i in 1..n] ++
          [obj],
          input_order,
          indomain_min,
          complete
         ) 
         % satisfy;
         minimize obj;

constraint
  % note that x[i,j] = 1 leads to a redundant constraint 
  forall(i in 1..num_edges) (k[E[i,2]] - k[E[i,1]] >= 1 - num_edges * x[i])
  % /\
  % obj = 3
  % /\
  % GLPK:s solution
  % x = [0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0]
  % /\
  % k = [1,2,3,4,15,14,14,4,15,1,2,1,3,14,3]



;


% 
% data
% 

% """
% The optimal solution is 3 
% """

n = 15;
num_edges = 23;
E = array2d(1..num_edges, 1..2, 
[1,2,
2,3,
3,4,
3,8,
4,9,
5,1,
6,5,
7,5,
8,6,
8,7,
8,9,
9,10,
10,11,
10,14,
11,15,
12,7,
12,8,
12,13,
13,8,
13,12,
13,14,
14,9,
15,14]);



output
[
  "x: " ++ show(x) ++ "\n" ++
  "obj: " ++ show(obj) ++ "\n" 
];
