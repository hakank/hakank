% 
% All different with "explanation" in MiniZinc.
% 
% This model shows which values that are inconsistent with the all different
% constraint. Thus this constraint will never fail.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..n: x;
var set of 1..n: s; % this contain the values that conflicts with alldifferent
var 0..n: num_conflicts;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

predicate alldifferent_explain(array[int] of var int: a, var set of int: d) =

   forall(i in lb_array(x)..ub_array(x)) (
     ( % remove values not in x
      not(exists(j in index_set(x)) (
        x[j] = i
      )) -> not(i in d)
     )
     /\
     ( % mark duplicates in s
      exists(a,b in index_set(x) where a < b) (
        x[a] = i /\ x[b] = i 
      ) <-> i in d
     )
   )
;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   % s will be {4}
   % cp1d(x, [1,4,3,4,4])
   % /\

   alldifferent_explain(x, s)

   /\
   num_conflicts = card(s)

   % /\ num_conflicts = 0 % alldifferent (i.e. no duplicates)

   /\ num_conflicts <= 1 % we accept atmost 1 duplicated value
;

output [
  "x: " ++ show(x), "\n" ++
  "s: " ++ show(s), "\n" ++
  "num_conflicts: " ++ show(card(s))
];
