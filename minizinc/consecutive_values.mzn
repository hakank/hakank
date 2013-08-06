% 
% Global constraint consecutive_values in MiniZinc.
% 
% From Global Constraint Catalog:
% http://www.emn.fr/z-info/sdemasse/gccat/Cconsecutive_values.html
% """
% Purpose:
% Constraint the difference between the largest and the smallest values of the 
% VARIABLES collection to be equal to the number of distinct values assigned 
% to the variables of the VARIABLES collection minus one (i.e., there is no holes 
% at all within the used values).
% 
% Example: (5,4,3,5)
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
int: m = 10;

array[1..n] of var 1..10: x;

predicate consecutive_values(array[int] of var int: x) =
   let {
      var 1..length(x): nv
   } in
   nvalue(nv, x)
   /\
   max(x) - min(x) = nv - 1
;

% copy 
predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % cp1d([5,4,3,5], x) /\ % should be accepted
  % cp1d([1,4,3,5], x) /\   % should fail
  consecutive_values(x)
;

output [ show(x) ++ "\n"];


