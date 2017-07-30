% 
% Global constraint balance in MiniZinc.
% 
% From Global Constraint Catalogue:
% http://www.emn.fr/x-info/sdemasse/gccat/Cbalance.html
% """
% BALANCE is equal to the difference between the number of occurrence of the 
% value that occurs the most and the value that occurs the least within the 
% collection of variables VARIABLES
% 
% Example
%    (2,<3,1,7,1,1>)
%
% In this example, values 1,​3 and 7 are respectively used 3,​1 and 1 times. 
% The balance constraint holds since its first argument BALANCE is assigned to 
% the difference between the maximum and minimum number of the previous 
% occurrences (i.e., 3−1).
%
% """
%

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..7: x;
var int: bal;

array[1..7] of var 0..n: gcc;

predicate min_except_0(var int: min_val, array[int] of var int: x) =

     exists(i in index_set(x)) (
       min_val = x[i] /\ 
       forall(j in index_set(x) where i != j) (
         x[i] <= x[j]
         \/ % ignore 0
         x[j] = 0
       )
     )
     /\
     min_val > 0
;


predicate balance(var int: bal, array[int] of var int: x) =
   let {
     int: lbx = lb_array(x),
     int: ubx = ub_array(x),
     array[lbx..ubx] of var 0..ubx: counts,
     var 0..ubx: x_max,
     var 0..ubx: x_min
   }
   in
   global_cardinality(x, set2array(index_set(counts)), counts)
   /\ 
   x_max = max(counts)
   /\
   min_except_0(x_min, counts)
   /\
   bal = x_max - x_min % get the difference (the "balance")
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


solve :: int_search(x ++ [bal], first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
  %cp1d([3,1,7,1,1],x)
  %/\
  balance(bal, x)
  /\
  bal = 3
;

constraint global_cardinality(x, [i|i in 1..7], gcc);

output [
  "bal: " ++ show(bal) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "gcc: " ++ show(gcc) 
];
