% 
% Graceful labeling problem in MiniZinc.
% 
% From
% Belaid MOA, "Examples for Discrete Constraint Programming"
% http://www.docstoc.com/docs/23988870/Examples-for-Discrete-Constraint-Programming
% page 55ff
% """
% Given a tree T with m+1 nodes, and m edges find the possible labels in
% {0,1,...m} for the nodes such that the absolute value of the difference
% of the labels of the nodes related to each edge are all different. 
% """
%
% The example below is from page 58.
% There are 60 solutions to this problem. The solution shown on page 58
% is [0,6,1,3,4,5,2].
%

%
% Compare with the K4P2 Graceful graph model:
%  http://www.hakank.org/minizinc/K4P2GracefulGraph.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn"; 

int: nbreOfNodes;
set of 1..nbreOfNodes: indices = 1..nbreOfNodes; 
array[indices,indices] of 0..1: adjacencyMatrix;

set of 0..nbreOfNodes-1: nodes = 0..nbreOfNodes-1;
array[indices] of var nodes: labels;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
solve :: int_search(labels, first_fail, indomain_min, complete) satisfy;

constraint
   all_different(labels) /\
   forall(i,j in indices where i < j) (
     if adjacencyMatrix[i,j] > 0 then
        forall(k,h in indices where k < h) (
             if adjacencyMatrix[k,h] > 0 then
                  not(k=i /\ h=j) -> (abs(labels[i]-labels[j]) != abs(labels[h]-labels[k]))
              else 
                 true
              endif
        )
     else
       true
     endif
   )

   % /\ cp1d(labels, [0,6,1,3,4,5,2]) % the example solution
;

%
% data
%
nbreOfNodes = 7;
adjacencyMatrix = array2d(indices,indices,
[
 0,1,0,0,0,0,0,
 1,0,1,0,0,0,0,
 0,1,0,1,0,1,0,
 0,0,1,0,1,0,0,
 0,0,0,1,0,0,0,
 0,0,1,0,0,0,1,
 0,0,0,0,0,1,0
]);

output [
  "labels: " ++ show(labels) ++ "\n"
];
