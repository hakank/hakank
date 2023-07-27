% 
% Global constraint symmetric in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csymmetric.html
% """
% Consider a digraph G described by the NODES collection. Select a subset of arcs 
% of G so that the corresponding graph is symmetric (i.e., if there is an arc from 
% i to j, there is also an arc from j to i).
% 
% Example
%   (
%   <
%    index-1 succ-{1,2,3},
%    index-2 succ-{1,3}​,
%    index-3 succ-{1,2}​,
%    index-4 succ-{5,6}​,
%    index-5 succ-{4},
%    index-6 succ-{4}
%    >
%    )
%     The symmetric constraint holds since the NODES collection depicts a symmetric graph.

% """

% 
% MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 


int: n = 6;
array[1..n] of var set of 1..n: x;


predicate symmetric(array[int] of var set of int: s) =

  forall(i, j in index_set(s) where i != j) (
    j in s[i] <-> i in s[j]
  )
;

predicate cp1d(array[int] of var set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
   cp1d(x, 
       [
        {1,2,3},
        {1,3},
        {1,2},
        {5,6},
        {4},
        {4}
    ])
  /\
  symmetric(x)

;


output [
  "x: ", show(x), "\n"
];
