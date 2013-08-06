% 
% Graph degree sequence in MiniZinc.
% 
% Given a (directed) graph as matrix gives the degree sequence.
% 
% Reference:
% http://mathworld.wolfram.com/DegreeSequence.html
%

%
% If both d and graph is uninitialized we see that for n = 4 there are 
% 65536 possible (directed) graphs (65536 = 2^16).
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n;
array[1..n] of var 0..n: d; % degree sequence
array[1..n, 1..n] of var 0..1: graph;  % matrix version, directed

% edges version
% int: num_edges;
% array[1..num_edges, 1..2] of var 1..n: graph; 


%
% Matrix version
%
predicate degree_sequence_matrix(array[int] of var int: d_seq, array[int, int] of var 0..1: g) =
   
   forall(i in index_set_1of2(g)) (
      d[i] = sum(j in index_set_2of2(g)) (bool2int(graph[i,j] = 1))
   )

;


%
% Edge version
%
predicate degree_sequence_edges(array[int] of var int: d_seq, array[int, 1..2] of var int: g, set of int: nodes) = 
    forall(i in nodes) (
      d_seq[i] = sum(j in 1..card(index_set_1of2(g))) ( 
          bool2int(g[j,1] = i)
      )
    )
;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;


constraint

    % matrix version
    cp2d(graph, array2d(1..n, 1..n, 
    [
      1,0,1,0,
      1,1,0,1,
      0,1,1,0,
      0,0,1,1
    ]))
    /\
    degree_sequence_matrix(d, graph)
    % /\
    % cp1d(d, [2,3,2,2])

    % edge version
    % cp2d(graph, array2d(1..num_edges, 1..2,
    % [
    % 1,2,
    % 1,3,
    % 1,4,
    % 2,3,
    % 2,4,
    % 3,4,
    % ]))
    % /\
    % cp1d(d,[3,2,1,0])
    % /\
    % degree_sequence_edges(d, graph, 1..n)
;

output [
  "degrees: ", show(d), "\n",
  "graph:"
] 
++  
[ % the matrix version
  if j = 1 then "\n" else " " endif ++
     show(graph[i,j])
  | i,j in 1..n
]
% [ %  the edge version
%  if j = 1 then "\n" else " " endif ++
%    show(graph[i,j])
%  | i in 1..num_edges, j in 1..2
%]
++ ["\n"]
;

%
% data
%
n = 4;
% num_edges = 6;
