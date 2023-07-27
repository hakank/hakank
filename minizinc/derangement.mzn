% 
% Global constraint derangement in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cderangement.html
% """
% Enforce to have a permutation with no cycle of length one. The permutation 
% is depicted by the succ attribute of the NODES collection.
% 
% Example
%     (
%    <
%    index-1	succ-2,
%    index-2	succ-1,
%    index-3	succ-5,
%    index-4	succ-3,
%    index-5	succ-4
%    >
%    )
% In the permutation of the example we have the following 2 cycles: 1->2->1 and
% 3->5->4->3. Since these cycles have both a length strictly greater than 
% one the corresponding derangement constraint holds.

% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..n: x;

predicate derangement(array[int] of var int: x) = 
   let {
     int: n = length(x)
   }
   in
   forall(i in 1..n) (
     x[i] != i
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

solve satisfy;

constraint

  cp1d([2,1,5,3,4], x)
  /\
  derangement(x)

;

output
[
  show(x)
];
