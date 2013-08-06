% 
% Shortest Path Problem in MiniZinc.
% 
% From GLPK example spp.mod
% """
% SPP, Shortest Path Problem 
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru> 
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Given a directed graph G = (V,E), its edge lengths c(i,j) for all
% (i,j) in E, and two nodes s, t in V, the Shortest Path Problem (SPP)
% is to find a directed path from s to t whose length is minimal. */
array[1..num_edges, 1..2] of 1..n: E; 

% number of nodes 
int: n; % , integer, > 0;


int: num_edges;


% c[i,j] is length of edge (i,j); note that edge lengths are allowed
%   to be of any sign (positive, negative, or zero)
array[1..num_edges] of int: c;

% source node 
1..n: s;

% target node 
1..n: t;


% """
% x[i,j] = 1 means that edge (i,j) belong to shortest path;
% x[i,j] = 0 means that edge (i,j) does not belong to shortest path;
% note that variables x[i,j] are binary, however, there is no need to
% declare them so due to the totally unimodular constraint matrix
% """
array[1..num_edges] of var 0..1: x;


% objective function is the path length to be minimized 
var int: z = sum(i in 1..num_edges) (c[i] * x[i]);

solve minimize z;
% solve satisfy;


constraint 

  % """
  % conservation conditions for unity flow from s to t; every feasible
  % solution is a path from s to t
  % """
  forall(i in 1..n) (
         sum(k in 1..num_edges where E[k,2] = i) (x[k]) + (if i = s then 1 else 0 endif)
    = 
         sum(k in 1..num_edges where E[k,1] = i) (x[k]) + (if i = t then 1 else 0 endif)
  )
;

%
% data
%

% """
% Optimal solution is 20 that corresponds to the following shortest
% 
% path: s = 1 -> 2 -> 4 -> 8 -> 6 = t
% """
% I.e. edges [1, 4, 11, 14] is the shortest path
% 
n = 8;
s = 1;
t = 6;
num_edges = 15;
E = array2d(1..num_edges, 1..2, [
       1, 2, % 1  *
       1, 4, % 2 
       1, 7, % 3
       2, 4, % 4  *
       3, 2, % 5
       3, 4, % 6
       3, 5, % 7
       3, 6, % 8
       4, 5, % 9
       4, 8, % 10 *
       5, 8, % 11
       6, 5, % 12
       7, 4, % 13
       8, 6, % 14 *
       8, 7  % 15
]);

c = [1, 8, 6, 2, 14, 10, 6, 19, 8, 13, 12, 7, 5, 4, 10];


output
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];