% 
% Traveling Salesman Problem, integer programming model in MiniZinc.
% 
% From GLPK:s example tsp.mod
% """
% TSP, Traveling Salesman Problem
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru> */
%
% The Traveling Salesman Problem (TSP) is stated as follows.
% Let a directed graph G = (V, E) be given, where V = {1, ..., n} is
% a set of nodes, E <= V x V is a set of arcs. Let also each arc
% e = (i,j) be assigned a number c[i,j], which is the length of the
% arc e. The problem is to find a closed path of minimal length going
% through each node of G exactly once.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Note: 
% Solving with ECLiPSe/eplex (occurrence, indomain_min, complete) took about 6.5 minutes:
% total = 6859
% x = [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0]
% y = [0,0,0,0,0,0,0,0,0,0,0,0,15,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0]
% Minimum objective value = 6859.0000000000009
%

% number of nodes 
int: n;

% set of arcs 
int: num_edges;
array[1..num_edges, 1..2] of 1..n: E;

% distance from node i to node j 
array[1..num_edges] of int: c;

% x[i,j] = 1 means that the salesman goes from node i to node j 
array[1..num_edges] of var 0..1: x;

% y[i,j] is the number of cars, which the salesman has after leaving
% node i and before entering node j; in terms of the network analysis,
% y[i,j] is a flow through arc (i,j) 
array[1..num_edges] of var int: y;

% the objective is to make the path length as small as possible 
var int: total = sum(i in 1..num_edges) (c[i] * x[i]);
solve :: int_search(
    [x[i] | i in 1..num_edges] ++
    [y[i] | i in 1..num_edges] ++
    [total],
   first_fail, % "occurrence",
   indomain_max,
   complete
   )
   minimize total;

constraint

   % the salesman leaves each node i exactly once 
   forall(i in 1..n) (
        sum(k in 1..num_edges where E[k,1] = i) (x[k]) = 1
   )

   /\

   % the salesman enters each node j exactly once 
   forall(j in 1..n) (
        sum(k in 1..num_edges where E[k,2] = j) (x[k]) = 1
   )

   /\

   % """
   % Constraints above are not sufficient to describe valid tours, so we
   % need to add constraints to eliminate subtours, i.e. tours which have
   % disconnected components. Although there are many known ways to do
   % that, I invented yet another way. The general idea is the following.
   % Let the salesman sells, say, cars, starting the travel from node 1,
   % where he has n cars. If we require the salesman to sell exactly one
   % car in each node, he will need to go through all nodes to satisfy
   % this requirement, thus, all subtours will be eliminated. 
   % 

   % if arc (i,j) does not belong to the salesman's tour, its capacity
   % must be zero; it is obvious that on leaving a node, it is sufficient
   % to have not more than n-1 cars 
   % """
   forall(k in 1..num_edges) (
      y[k] >= 0
      /\
      y[k] <= (n-1) * x[k]
   )

   /\
   % node[i] is a conservation constraint for node i 
   forall(i in 1..n) (
      % summary flow into node i through all ingoing arcs 
      (
      sum(k in 1..num_edges where E[k,2] = i) (y[k])
      % plus n cars which the salesman has at starting node 
      + (if i = 1 then n else 0 endif)
      )
      = % must be equal to 
      % summary flow from node i through all outgoing arcs 
      (
      sum(k in 1..num_edges where E[k,1] = i) (y[k])
      % plus one car which the salesman sells at node i 
      + 1
      )
   )

;

output [
   "total: " ++ show(total) ++ "\n" ++
   "x: " ++ show(x) ++ "\n" ++
   "y: " ++ show(y) ++ "\n"

];

% 
% data
% 

% """
% These data correspond to the symmetric instance ulysses16 from:
% Reinelt, G.: TSPLIB - A travelling salesman problem library.
% ORSA-Journal of the Computing 3 (1991) 376-84;
% http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
% 
% The optimal solution is 6859
% """

n = 16;
num_edges = 240;

E = array2d(1..num_edges, 1..2, 
[
1,2,
1,3,
1,4,
1,5,
1,6,
1,7,
1,8,
1,9,
1,10,
1,11,
1,12,
1,13,
1,14,
1,15,
1,16,
2,1,
2,3,
2,4,
2,5,
2,6,
2,7,
2,8,
2,9,
2,10,
2,11,
2,12,
2,13,
2,14,
2,15,
2,16,
3,1,
3,2,
3,4,
3,5,
3,6,
3,7,
3,8,
3,9,
3,10,
3,11,
3,12,
3,13,
3,14,
3,15,
3,16,
4,1,
4,2,
4,3,
4,5,
4,6,
4,7,
4,8,
4,9,
4,10,
4,11,
4,12,
4,13,
4,14,
4,15,
4,16,
5,1,
5,2,
5,3,
5,4,
5,6,
5,7,
5,8,
5,9,
5,10,
5,11,
5,12,
5,13,
5,14,
5,15,
5,16,
6,1,
6,2,
6,3,
6,4,
6,5,
6,7,
6,8,
6,9,
6,10,
6,11,
6,12,
6,13,
6,14,
6,15,
6,16,
7,1,
7,2,
7,3,
7,4,
7,5,
7,6,
7,8,
7,9,
7,10,
7,11,
7,12,
7,13,
7,14,
7,15,
7,16,
8,1,
8,2,
8,3,
8,4,
8,5,
8,6,
8,7,
8,9,
8,10,
8,11,
8,12,
8,13,
8,14,
8,15,
8,16,
9,1,
9,2,
9,3,
9,4,
9,5,
9,6,
9,7,
9,8,
9,10,
9,11,
9,12,
9,13,
9,14,
9,15,
9,16,
10,1,
10,2,
10,3,
10,4,
10,5,
10,6,
10,7,
10,8,
10,9,
10,11,
10,12,
10,13,
10,14,
10,15,
10,16,
11,1,
11,2,
11,3,
11,4,
11,5,
11,6,
11,7,
11,8,
11,9,
11,10,
11,12,
11,13,
11,14,
11,15,
11,16,
12,1,
12,2,
12,3,
12,4,
12,5,
12,6,
12,7,
12,8,
12,9,
12,10,
12,11,
12,13,
12,14,
12,15,
12,16,
13,1,
13,2,
13,3,
13,4,
13,5,
13,6,
13,7,
13,8,
13,9,
13,10,
13,11,
13,12,
13,14,
13,15,
13,16,
14,1,
14,2,
14,3,
14,4,
14,5,
14,6,
14,7,
14,8,
14,9,
14,10,
14,11,
14,12,
14,13,
14,15,
14,16,
15,1,
15,2,
15,3,
15,4,
15,5,
15,6,
15,7,
15,8,
15,9,
15,10,
15,11,
15,12,
15,13,
15,14,
15,16,
16,1,
16,2,
16,3,
16,4,
16,5,
16,6,
16,7,
16,8,
16,9,
16,10,
16,11,
16,12,
16,13,
16,14,
16,15
]);


c=[
509,
501,
312,
1019,
736,
656,
60,
1039,
726,
2314,
479,
448,
479,
619,
150,
509,
126,
474,
1526,
1226,
1133,
532,
1449,
1122,
2789,
958,
941,
978,
1127,
542,
501,
126,
541,
1516,
1184,
1084,
536,
1371,
1045,
2728,
913,
904,
946,
1115,
499,
312,
474,
541,
1157,
980,
919,
271,
1333,
1029,
2553,
751,
704,
720,
783,
455,
1019,
1526,
1516,
1157,
478,
583,
996,
858,
855,
1504,
677,
651,
600,
401,
1033,
736,
1226,
1184,
980,
478,
115,
740,
470,
379,
1581,
271,
289,
261,
308,
687,
656,
1133,
1084,
919,
583,
115,
667,
455,
288,
1661,
177,
216,
207,
343,
592,
60,
532,
536,
271,
996,
740,
667,
1066,
759,
2320,
493,
454,
479,
598,
206,
1039,
1449,
1371,
1333,
858,
470,
455,
1066,
328,
1387,
591,
650,
656,
776,
933,
726,
1122,
1045,
1029,
855,
379,
288,
759,
328,
1697,
333,
400,
427,
622,
610,
2314,
2789,
2728,
2553,
1504,
1581,
1661,
2320,
1387,
1697,
1838,
1868,
1841,
1789,
2248,
479,
958,
913,
751,
677,
271,
177,
493,
591,
333,
1838,
68,
105,
336,
417,
448,
941,
904,
704,
651,
289,
216,
454,
650,
400,
1868,
68,
52,
287,
406,
479,
978,
946,
720,
600,
261,
207,
479,
656,
427,
1841,
105,
52,
237,
449,
619,
1127,
1115,
783,
401,
308,
343,
598,
776,
622,
1789,
336,
287,
237,
636,
150,
542,
499,
455,
1033,
687,
592,
206,
933,
610,
2248,
417,
406,
449,
636,
];