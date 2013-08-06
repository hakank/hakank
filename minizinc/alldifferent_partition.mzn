% 
% Global constraint alldifferent_partition in MiniZinc.
% 
% From Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_partition.html
% """
% Enforce all variables of the collection VARIABLES to take values that 
% belong to distinct partitions.
% 
% Example
%    (
%    <6, 3, 4>,
%    <
%    p-<1, 3>,
%    p-<4>,
%    p-<2, 6>
%    >
%    )
%
% Since all variables take values that are located within distinct partitions 
% the alldifferent_partition constraint holds.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 3;
set of int: ss = {1,2,3,4,6};
array[1..n] of var ss: x;
array[1..n] of var set of ss: s; % the set of the "universe" ss

predicate all_different_partition(array[int] of var int: x, array[int] of var set of int: s, set of int: universe) =
   let {
      int: lbx = min(index_set(x)),
      int: ubx = max(index_set(x))
   } 
   in
   partition_set(s, universe)
   /\
   all_different(x)
   /\
   forall(i in lbx..ubx) (
      sum(j in lbx..ubx) ( 
         bool2int(x[i] in s[j])
      ) = 1
   )
   /\
   forall(i in lbx..ubx) (
      sum(j in lbx..ubx) (
         bool2int(x[j] in s[i])
      ) <= 1
   )

;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 

% copy, set variant
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
   cp1d([6,3,4], x)
   /\
   cp1d([{1,3}, {4}, {2,6}], s)
   /\
   all_different_partition(x, s, ss)

;


output [
"x: " ++ show(x) ++ "\n"++
"s: " ++ show(s) ++ "\n"++
"s: " ++ show(s) ++ "\n"
];
