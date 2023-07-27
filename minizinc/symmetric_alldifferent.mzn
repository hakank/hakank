% 
% Global constraint symmetric_alldifferent in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csymmetric_alldifferent.html
% """
% All variables associated with the succ attribute of the NODES collection should be 
% pairwise distinct. In addition enforce the following condition: if variable 
% NODES[i].succ takes value j then variable NODES[j].succ takes value i. 
% This can be interpreted as a graph-covering problem where one has to cover a 
% digraph G with circuits of length two in such a way that each vertex of G belongs 
% to one single circuit.
% 
% Example
% (
% <
% index-1 succ-3,
% index-2 succ-4,
% index-3 succ-1,
% index-4 succ-2
% >
% )
% 
% The symmetric_alldifferent constraint holds since:
%   * NODES[1].succ=3 <-> NODES[3].succ=1,
%   * NODES[2].succ=4 <-> NODES[4].succ=2.
% """

% Note: In MiniZinc 2.0, symmetric_all_different is a built-in constraint.

% 
% MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..n: x;

%
% Note: This version do not assume that a is of even length, which
% means that fixpoints is allowed. E.g. the following are the four
% solutions for n = 3 (where there are at least one fixpoint):
%
% x: [1, 2, 3]
% x: [1, 3, 2]
% x: [2, 1, 3]
% x: [3, 2, 1]
%
predicate symmetric_alldifferent_me(array[int] of var int: a) =
   all_different(x)
   /\
   forall(i in index_set(a)) (
      x[x[i]] = i
      % /\ x[i] != i % add this if no fixpoint is allowed
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint 
   %cp1d(x, [3,4,1,2])
   %/\
   symmetric_alldifferent_me(x)
   % symmetric_all_different(x) % built-in
;


output [
  "x: ", show(x), "\n"
];
