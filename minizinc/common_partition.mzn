% 
% Global constraint common_partition in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Ccommon_partition.html
% """
% Constraint
%
% common_partition (NCOMMON1, NCOMMON2, VARIABLES1, VARIABLES2, PARTITIONS) 
%
% Purpose
%
% NCOMMON1 is the number of variables of the VARIABLES1 collection 
% taking a value in a partition derived from the values assigned to 
% the variables of VARIABLES2 and from PARTITIONS.
%
% NCOMMON2 is the number of variables of the VARIABLES2 collection 
% taking a value in a partition derived from the values assigned to 
% the variables of VARIABLES1 and from PARTITIONS.
%
% Example
%     (
%     3, 4, <2, 3, 6, 0>, 
%     <0, 6, 3, 3, 7, 1>, 
%     <
%     p-<1, 3>, 
%     p-<4>, 
%     p-<2, 6>
%     >
%     )
%
% In the example, the last argument PARTITIONS defines the partitions 
% p-<1, 3>, p-<4> and p-<2, 6>. As a consequence the first three items 
% of collection <2, 3, 6, 0> respectively correspond to the partitions 
% p-<2, 6>, p-<1, 3>, and p-<2, 6>. Similarly the items of collection 
% <0, 6, 3, 3, 7, 1> (from which we remove items 0 and 7 since they do 
% not belong to any partition) respectively correspond to the partitions 
% p-<2, 6>, p-<1, 3>, p-<1, 3>, and p-<1, 3>. The common_partition 
% constraint holds since:
%
% * Its first argument NCOMMON1=3 is the number of partitions associated 
%   with the items of collection <2, 3, 6, 0> that also correspond to 
%   partitions associated with <0, 6, 3, 3, 7, 1>.
%
% * Its second argument NCOMMON2=4 is the number of partitions associated 
%   with the items of collection <0, 6, 3, 3, 7, 1> that also correspond 
%   to partitions associated with <2, 3, 6, 0>.
% """
% 

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..4] of var 1..6: x;
array[1..6] of var 1..7: y;
array[1..3] of var set of 0..6: partitions;

var int: a;
var int: b;

solve satisfy;


predicate common_partition(var int: a, var int: b, array[int] of var int: x, array[int] of var int: y, array[int] of var set of int: partitions) = 
   let {
     int: lbp = min(index_set(partitions)),
     int: ubp = max(index_set(partitions)),
     int: ubx = max(index_set(x)),
     int: uby = max(index_set(y)),
     array[lbp..ubp] of var 0..uby: x_count,
     array[lbp..ubp] of var 0..ubx: y_count
   }
   in
 
   % count the occurrences of each array in the partitions
   forall(i in index_set(partitions)) (
      x_count[i] = sum(j in index_set(x)) ( bool2int(x[j] in partitions[i]))
      /\
      y_count[i] = sum(j in index_set(y)) ( bool2int(y[j] in partitions[i]))
   )
   /\
   % I _think_ this is correct: Count the number of occurrences in this
   % slot if there are any occurrences in the other slot.
   a = sum(i in index_set(partitions)) (
           x_count[i]*bool2int(y_count[i] > 0)
       )
   /\
   b = sum(j in index_set(partitions) ) (
           y_count[j]*bool2int(x_count[j] > 0)
       )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

predicate cp1d(array[int] of set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
   cp1d([2,3,6,5], x) /\
   cp1d([5,6,3,3,7,1],y) /\

   cp1d(
   [
     {1,3},
     {4},
     {2,6}
   ], partitions)
   /\
   common_partition(a, b, x, y, partitions)

   % The "reverse problem" works as well, i.e. letting x and y be unknown
   % and fix a and b.
   % /\ a = 3
   % /\ b = 4

   % /\ increasing(x)
   % /\ increasing(y) 

   % /\ 
   % used_by(x, y)
   % /\ used_by(y, x)
;



 output [
"a: " ++ show(a) ++ "\n"++
"b: " ++ show(b) ++ "\n"++
"x: " ++ show(x) ++ "\n"++
"y: " ++ show(y) ++ "\n"++
"partitions: " ++ show(partitions) ++ "\n"
];
