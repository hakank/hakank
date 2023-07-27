% 
% Global constraint balance_partition in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Cbalance_partition.html
% """
%  balance_partition(BALANCE, VARIABLES, PARTITIONS)
%
% Purpose
%
% Consider the largest set S1 (respectively the smallest set S2) of 
% variables of the collection VARIABLES that take their value in the 
% same partition of the collection PARTITIONS.BALANCE is equal to the 
% difference between the cardinality of S2 and the cardinality of S1.
%
% Example
%    (
%     1, <6, 2, 6, 4, 4>,
%     <
%     p-<1, 3>,
%     p-<4>,
%     p.<2, 6>
%     >
%     )
%
% In this example values 6, 2, 6, 4, 4 are respectively associated 
% with the partitions p-<2, 6> and p-<4>. Partitions p-<4> and p-<2, 6> 
% are respectively used 2 and 3 times. The balance_partition constraint 
% holds since its first argument BALANCE is assigned to the difference 
% between the maximum and minimum number of the previous occurrences 
% (i.e., 3-2). Note that we don't consider those partitions that 
% are not used at all.
%
% Usage
%
% An application of the balance_partition is to enforce a balanced 
% assignment of values, no matter how many distinct partitions will be 
% used. In this case one will push down the maximum value of the 
% first argument of the balance_partition constraint.
% """
%

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..7: x;
array[1..3] of var set of 1..7: partitions;
var 0..n: bal;


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


predicate balance_partition(var int: bal, array[int] of var int: x, array[int] of var set of int: partitions) =
   let {
     int: lbp = min(index_set(partitions)),
     int: ubp = max(index_set(partitions)),
     int: ubx = max(index_set(x)),
     array[lbp..ubp] of var 0..ubx: counts,
     var 0..ubx: c_max,
     var 0..ubx: c_min
   }
   in
   forall(i in index_set(partitions)) (
      counts[i] = sum(j in index_set(x)) ( 
              bool2int(x[j] in partitions[i])
      )
   )
   /\ 
   c_max = max(counts)
   /\
   min_except_0(c_min, counts)
   /\
   bal = c_max - c_min
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 

predicate cp1d(array[int] of set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


solve satisfy;

constraint
  cp1d([6,2,6,4,4],x)
  /\
  cp1d(
  [ 
   {1,3},
   {4},
   {2,6}
  ], partitions)

  /\
  balance_partition(bal, x, partitions)
  %/\
  %bal = 2

;


output [
  "x: ", show(x), "\n",
  "bal: ", show(bal), "\n"
];
