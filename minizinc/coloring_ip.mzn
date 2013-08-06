% 
% Graph Coloring Problem (integer programming) in MiniZinc.
% 
%
% Inspired by the GLPK:s model color.mod
% """
% COLOR, Graph Coloring Problem
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>

% Given an undirected loopless graph G = (V, E), where V is a set of
% nodes, E <= V x V is a set of arcs, the Graph Coloring Problem is to
% find a mapping (coloring) F: V -> C, where C = {1, 2, ... } is a set
% of colors whose cardinality is as small as possible, such that
% F(i) != F(j) for every arc (i,j) in E, that is adjacent nodes must
% be assigned different colors.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% Note: The MathProg model color.mod uses an heuristic to minimize the number 
%       of colors to considerate, the parameter nc and z.
%
%       Since MiniZinc cannot handle dynamic arrays, I don't even
%       try to translate this heuristic, and the references to z is skipped,
%       hence nc is hardcoded.
%
%       Also note that MiniZinc don't support the nodes construct 
%       "within V cross V".
%
%       This is, however, (still) an integer program model.


% number of nodes 
int: n;

% set of nodes 
set of int: V = 1..n;


int: num_edges;
array[1..num_edges, 1..2] of V: E;


% hakank: EE and z is used as a heuristic for calculating 
%         the maximum number of colors used (parameter nc). 
%         All this is skipped.
%
% """
% We need to estimate an upper bound of the number of colors |C|.
% The number of nodes |V| can be used, however, for sparse graphs such
% bound is not very good. To obtain a more suitable estimation we use
% an easy "greedy" heuristic. Let nodes 1, ..., i-1 are already
% assigned some colors. To assign a color to node i we see if there is
% an existing color not used for coloring nodes adjacent to node i. If
% so, we use this color, otherwise we introduce a new color. 
% """
%

% z[i,0] = color index assigned to node i
% [
%   skipped the definition of param z
% ]


% """
% number of colors used by the heuristic; obviously, it is an upper
% bound of the optimal solution 
% """
% hakank: We know that 4 colors suffice to color the graph...
int: nc = 5;


% """
% x[i,c] = 1 means that node i is assigned color c 
% """
array[V, 1..nc] of var 0..1: x;

% """
% u[c] = 1 means that color c is used, i.e. assigned to some node 
% """
array[1..nc] of var 0..1: u;

% objective is to minimize the number of colors used 
var int: obj = sum(c in 1..nc) (u[c]);
solve minimize obj;

constraint

  % there must be no loops 
  forall(i in 1..num_edges) (
    E[i,1] != E[i,2]
  )

  /\
  % each node must be assigned exactly one color 
  forall(i in V) (sum(c in 1..nc) (x[i,c]) = 1)

  /\
  % adjacent nodes cannot be assigned the same color 
  forall(i in 1..num_edges, c in 1..nc) (
        x[E[i,1],c] + x[E[i,2],c] <= u[c]
  )

;

output [
  "obj: ", show(obj),"\n",
  "u: ", show(u), "\n",
] ++
[
  if j = 1 then "\n" ++ show(i) ++ ": "  else " " endif ++
    show(x[i,j])
  | i in V, j in 1..nc
] ++ ["\n"];


%
% data
% 

% The optimal solution is 4 

n = 11;
num_edges = 20;

%
% These data correspond to the instance myciel3.col from:
% http://mat.gsia.cmu.edu/COLOR/instances.html 
%
E = array2d(1..num_edges, 1..2, [
 1, 2,
 1, 4,
 1, 7,
 1, 9,
 2, 3,
 2, 6,
 2, 8,
 3, 5,
 3, 7,
 3, 10,
 4, 5,
 4, 6,
 4, 10,
 5, 8,
 5, 9,
 6, 11,
 7, 11,
 8, 11,
 9, 11,
 10, 11]);

