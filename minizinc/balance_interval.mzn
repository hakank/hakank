% 
% Global constraint balance_interval in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Cbalance_interval.html
% """
% Constraint
%
%     balance_interval(BALANCE, VARIABLES, SIZE_INTERVAL)
%
% Purpose
%
% Consider the largest set S1 (respectively the smallest set S2) of 
% variables of the collection VARIABLES that take their value in a same 
% interval [SIZE_INTERVAL*k,​SIZE_INTERVAL*k+SIZE_INTERVAL-1], where k 
% is an integer. BALANCE is equal to the difference between the 
% cardinality of S2 and the cardinality of S1.
%
% Example
% (3, <6, 4, 3, 3, 4>, 3)
%
% In the example, the third argument SIZE_INTERVAL=3 defines the following 
% family of intervals [3*k, 3*k+2], where k is an integer. Values 
% 6,4,3,3 and 4 are respectively located within intervals 
% [6, 8], [3, 5], [3, 5], [3, 5] and [3, 5]. Therefore intervals 
% [6, 8]​and [3, 5] are respectively used 1 and 4 times. 
% The balance_interval constraint holds since its first argument BALANCE 
% is assigned to the difference between the maximum and minimum number 
% of the previous occurrences (i.e., 4-1).
% Usage
%
% An application of the balance_interval constraint is to enforce a 
% balanced assignment of interval of values, no matter how many distinct 
% interval of values will be used. In this case one will push down the 
% maximum value of the first argument of the balance_interval constraint.
% """
%

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..6: x;
var int: bal;
int: interval_size = 3;



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


%
% Note: This assumes only positive integers.
%
predicate balance_interval(var int: bal, array[int] of var int: x, int: interval_size) =
  let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
     int: int_size = (ub_array(x) div interval_size)+1, % how many intervals
     array[1..int_size, 1..2] of var 0..ub_array(x)*2: intervals,
     array[1..int_size] of var 0..ubx: counts,
     var 0..ubx: c_max,
     var 0..ubx: c_min
  } in

  intervals[1,1] = 0
  /\
  intervals[1,2] = interval_size - 1 
  /\
  forall(i in 2..int_size) (
      intervals[i,1] = interval_size*(i-1) /\
      intervals[i,2] = (interval_size*i)-1
  )

  /\
  forall(j in 1..int_size) (
     counts[j] = sum(i in index_set(x)) (
                                         bool2int(x[i] >= intervals[j,1] /\
                                                  x[i] <= intervals[j,2])
                                        )
  )

  /\ % get the maximum value of counts
  c_max = max(counts)

  /\ % get the minimum value of counts > 0
  min_except_0(c_min, counts)
  /\
  bal = c_max - c_min % get the difference (the "balance")
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


solve satisfy;
% solve :: int_search(x ++ [bal], first_fail, indomain_min, complete) satisfy;

constraint
  % cp1d([6,4,3,3,4],x)
  % /\
  balance_interval(bal, x, interval_size)
  /\
  bal = 3

;


output [
  "x: ", show(x), "\n",
  "bal: ", show(bal), "\n"       
];
